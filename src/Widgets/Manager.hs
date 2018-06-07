module Widgets.Manager where
import Commons
import qualified Widgets.Pane as Pane
import qualified Widgets.Tab as Tab
import qualified Widgets.Entry as Entry
import qualified Widgets.Menu as Menu
import qualified Widgets.Prompt as Prompt

import Control.Lens
import Data.Set.Lens (setOf)
import Data.Maybe (isJust, fromJust)
import System.Process (callCommand)
import Control.Concurrent (forkFinally, ThreadId)
import Control.Exception (try, SomeException)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Brick.Main (continue, halt, suspendAndResume)
import Brick.Widgets.Core (hBox, vBox, vLimit, withBorderStyle)
import Brick.Types (Widget, BrickEvent(..), EventM, Next, ViewportType(..), Location(..))
import Brick.Widgets.Border (vBorder, hBorder)
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.BChan (BChan, writeBChan)
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.PointedList (PointedList, focus, delete, singleton, insert, withFocus, find)
import Data.List.PointedList.Circular (next, previous)

data State = State {
    _paneZipper :: PaneZipper,
    _lastPaneName :: PaneName,
    _bottomMenu :: Menu.Menu,
    _prompt :: Maybe Prompt.Prompt,
    _lastClickedEntry :: Maybe (PaneName, Int, UTCTime),
    _editorCommand :: String,
    _eventChan :: EChan Tab.Tab,
    _searchers :: (Int, Int)
  }
type PaneZipper = PointedList Pane.Pane

-- lenses
paneZipper :: Lens' State PaneZipper
paneZipper = lens _paneZipper (\state x -> state {_paneZipper = x})

lastPaneName :: Lens' State PaneName
lastPaneName = lens _lastPaneName (\state x -> state {_lastPaneName = x})

bottomMenu :: Lens' State Menu.Menu
bottomMenu = lens _bottomMenu (\state x -> state {_bottomMenu = x})

prompt :: Lens' State (Maybe Prompt.Prompt)
prompt = lens _prompt (\state x -> state {_prompt = x})

lastClickedEntry :: Lens' State (Maybe (PaneName, Int, UTCTime))
lastClickedEntry = lens _lastClickedEntry (\state x -> state {_lastClickedEntry = x})

editorCommand :: Lens' State String
editorCommand = lens _editorCommand (\state x -> state {_editorCommand = x})

eventChan :: Lens' State (EChan Tab.Tab)
eventChan = lens _eventChan (\state x -> state {_eventChan = x})

searchers :: Lens' State (Int, Int)
searchers = lens _searchers (\state x -> state {_searchers = x})

runningSNum :: Lens' State Int
runningSNum = searchers._1

maxSNum :: Lens' State Int
maxSNum = searchers._2

currentPane :: Lens' State Pane.Pane
currentPane = paneZipper.focus

currentTab :: Lens' State Tab.Tab
currentTab = currentPane . Pane.currentTab

menuType :: Lens' State Menu.MenuType
menuType = bottomMenu . Menu.menuType

clipboard :: Lens' State Menu.Clipboard
clipboard = bottomMenu . Menu.clipboard

entries :: Traversal' State Entry.Entry
entries = paneZipper.traverse.Pane.entries

waitingEntries :: Traversal' State Entry.Entry
waitingEntries = entries.filtered Entry.isWaiting

-- creation
makeState :: FilePath -> String -> EChan Tab.Tab -> Int -> IO State
makeState path editCom eChan tNum = do
  pane <- Pane.make 0 path
  let srcs = (0, if tNum < 1 then 1 else tNum)
      state = State (singleton pane) 0 Menu.make Nothing Nothing editCom eChan srcs
  startSearchers (snd srcs) state

-- rendering
render :: State -> [Widget Name]
render state = case view prompt state of
  Just pr -> [Prompt.render pr, renderMain state]
  _ -> [renderMain state]

renderMain :: State -> Widget Name
renderMain state = vBox [
    views paneZipper renderPanes state,
    withBorderStyle unicodeBold hBorder,
    vLimit 3 $ Menu.render (state ^. bottomMenu) (state ^. currentPane)
  ]

renderPanes :: PaneZipper -> Widget Name
renderPanes = hBox . intersperse vBorder . map Pane.render . toList . withFocus

-- event handling
handleEvent :: State -> BrickEvent Name (ThreadEvent Tab.Tab) -> EventM Name (Next State)
handleEvent state ev
  | isSizeEvent ev = handleSize ev state
  | isJust $ view prompt state = handlePrompt ev state
  | otherwise = handleMain ev state

isSizeEvent :: BrickEvent Name (ThreadEvent Tab.Tab) -> Bool
isSizeEvent (AppEvent ev) = case ev of
  SizeFound _ _ -> True
  SizeNotFound _ -> True
  _ -> False
isSizeEvent _ = False

handleSize :: BrickEvent Name (ThreadEvent Tab.Tab) -> State -> EventM Name (Next State)
handleSize (AppEvent ev) state = case ev of
  SizeFound path val -> manageSearchers $ notifySize state path (Entry.Known val)
  SizeNotFound path -> manageSearchers $ notifySize state path Entry.Unknown
  _ -> continue state
handleSize _ state = continue state

--updates the prompt or replaces the current tab and closes the prompt
handlePrompt :: BrickEvent Name (ThreadEvent Tab.Tab) -> State -> EventM Name (Next State)
handlePrompt ev state = do
  promptRes <- Prompt.handleEvent ev (fromJust $ view prompt state) (view eventChan state)
  case promptRes of
    Left pr -> continue $ state & prompt ?~ pr
    Right tab -> manageSearchers $ state & currentTab .~ tab & prompt .~ Nothing

handleMain :: BrickEvent Name (ThreadEvent Tab.Tab) -> State -> EventM Name (Next State)
handleMain (VtyEvent ev) = case ev of
  EvKey KEsc [] -> halt
  EvKey KBS [] -> continue . set menuType Menu.Main
  EvKey (KChar 'l') [] -> continue . set menuType Menu.Selection
  EvKey (KChar 'a') [] -> continue . set menuType Menu.Tab
  EvKey (KChar 'p') [] -> continue . set menuType Menu.Pane
  EvKey (KChar 'q') [] -> halt
  EvKey (KChar 'x') [MCtrl] -> withSelectedEntry (set clipboard . Menu.CutBoard)
  EvKey (KChar 'c') [MCtrl] -> withSelectedEntry (set clipboard . Menu.CopyBoard)
  EvKey (KChar 'v') [MCtrl] -> openPromptWithClip Prompt.paste
  EvKey (KChar 'r') [MCtrl] -> openPrompt Prompt.rename
  EvKey (KChar 'd') [MCtrl] -> openPrompt Prompt.delete
  EvKey (KChar 'o') [MCtrl] -> openDirEntry True
  EvKey (KChar 's') [MCtrl] -> openPrompt Prompt.search
  EvKey (KChar 's') [] -> openPrompt Prompt.displayInfo
  EvKey (KChar 'm') [] -> openPrompt Prompt.mkdir
  EvKey (KChar 't') [] -> openPrompt Prompt.touch
  EvKey (KChar 'g') [] -> openPrompt Prompt.goTo
  EvKey KEnter [] -> openEntry
  EvKey (KChar 'e') [MCtrl] -> addPane
  EvKey (KChar 'k') [MCtrl] -> closePane
  EvKey KLeft [] -> continue . over paneZipper previous
  EvKey KRight [] -> continue . over paneZipper next
  _ -> manageSearchers <=< currentPane (Pane.handleEvent ev)
handleMain (MouseUp name _ (Location pos)) = case name of
  EntryList {pnName = pName} -> clickedEntry pName (snd pos)
  Label {pnName = pName, labelNum = n} -> continue . over currentPane (Pane.moveToNth n) . focusOnPane pName
  Button {keyBind = key, withCtrl = b} -> handleMain . VtyEvent $ EvKey key [MCtrl | b]
  _ -> continue
handleMain _ = continue

-- state-changing
withSelectedEntry :: (Entry.Entry -> State -> State) -> State -> EventM Name (Next State)
withSelectedEntry func state = continue . maybe state (`func` state) $ selectedEntry state

openPrompt :: (Tab.Tab -> PaneName -> Prompt.Prompt) -> State -> EventM Name (Next State)
openPrompt func state = continue $ state & prompt ?~ func tab pName
  where
    tab = view currentTab state
    pName = state ^. currentPane . Pane.name

openPromptWithClip :: (Menu.Clipboard -> Tab.Tab -> PaneName -> Prompt.Prompt) -> State -> EventM Name (Next State)
openPromptWithClip func state = openPrompt (func $ view clipboard state) state

clickedEntry :: PaneName -> Int -> State -> EventM Name (Next State)
clickedEntry pName row state = do
  currTime <- liftIO getCurrentTime
  let clicked = (pName, row, currTime)
      doubleClick = isDoubleClick (view lastClickedEntry state) clicked
  if doubleClick then openEntry $ state & lastClickedEntry .~ Nothing
  else continue . over currentTab (Tab.moveToRow row) . focusOnPane pName $ state & lastClickedEntry ?~ clicked

isDoubleClick :: Maybe (PaneName, Int, UTCTime) -> (PaneName, Int, UTCTime) -> Bool
isDoubleClick lastClick (nPane, nRow, nTime) = case lastClick of
  Nothing -> False
  Just (pPane, pRow, pTime) -> (pPane == nPane) && (pRow == nRow) && isSmallDiff
    where isSmallDiff = toRational (diffUTCTime nTime pTime) <= toRational 0.2

openEntry :: State -> EventM Name (Next State)
openEntry state = case selectedEntry state of
  Just Entry.Dir {} -> openDirEntry False state
  Just (Entry.File n p i) -> openFileEntry (Entry.File n p i) state
  _ -> continue state

openFileEntry :: Entry.Entry -> State -> EventM Name (Next State)
openFileEntry fileEntry
  | Entry.isExecutable fileEntry = suspendAndResume . runExternal (view Entry.path fileEntry)
  | Entry.isReadable fileEntry = suspendAndResume . runExternalEditor (view Entry.path fileEntry)
  | otherwise = continue

runExternalEditor :: FilePath -> State -> IO State
runExternalEditor path s = runExternal (unwords [view editorCommand s, path]) s

runExternal :: String -> State -> IO State
runExternal com s = do
  try $ callCommand com :: IO (Either SomeException ())
  putStrLn " "
  putStrLn "Done. Press ENTER to go back to clifm"
  getLine
  return s

openDirEntry :: Bool -> State -> EventM Name (Next State)
openDirEntry inNew = manageSearchers <=< currentPane (Pane.openDirEntry inNew)

addPane :: State -> EventM Name (Next State)
addPane state = continue $ state
  & paneZipper %~ insert (Pane.empty newName)
  & lastPaneName .~ newName
  where newName = 1 + view lastPaneName state

closePane :: State -> EventM Name (Next State)
closePane state = continue $ case delete $ view paneZipper state of
  Just newZipper -> state & paneZipper .~ newZipper
  _ -> state

-- directory entry size / threaded searchers / value update
manageSearchers :: State -> EventM Name (Next State)
manageSearchers state
  | running >= maxNum = continue state
  | otherwise = continue =<< liftIO (startSearchers (maxNum - running) state)
  where (running, maxNum) = view searchers state

startSearchers :: Int -> State -> IO State
startSearchers n state = do
  let toStart = Set.take n $ setOf waitingEntries state
  mapM_ (startSearcher (view eventChan state) . view Entry.path) $ Set.toList toStart
  return $ state
    & entries.filtered (`Set.member` toStart).Entry.size .~ Entry.Calculating
    & runningSNum +~ Set.size toStart

startSearcher :: EChan Tab.Tab -> FilePath -> IO ThreadId
startSearcher eChan path = forkFinally (Entry.getDirSize path) (reportSearch path eChan)

reportSearch :: FilePath -> EChan Tab.Tab -> Either SomeException Integer -> IO ()
reportSearch path eChan = writeBChan eChan . either (const (SizeNotFound path)) (SizeFound path)

notifySize :: State -> FilePath -> Entry.Size -> State
notifySize state path size = state
  & entries.filtered ((== path) . view Entry.path).Entry.size .~ size
  & runningSNum -~ 1
  & prompt %~ fmap (Prompt.notifySize path size)

-- pane and paneZipper utility
selectedEntry :: State -> Maybe Entry.Entry
selectedEntry = Pane.selectedEntry . view currentPane

focusOnPane :: PaneName -> State -> State
focusOnPane pName state = case views paneZipper (find (Pane.empty pName)) state of
  Just newZipper -> state & paneZipper .~ newZipper
  _ -> state
