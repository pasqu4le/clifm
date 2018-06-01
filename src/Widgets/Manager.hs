module Widgets.Manager where
import Commons
import qualified Widgets.Pane as Pane
import qualified Widgets.Tab as Tab
import qualified Widgets.Entry as Entry
import qualified Widgets.Menu as Menu
import qualified Widgets.Prompt as Prompt

import System.Process (callCommand)
import Control.Concurrent (forkFinally, ThreadId)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Brick.Main (continue, halt, suspendAndResume)
import Brick.Widgets.Core ((<+>), str, hBox, vBox, vLimit, withBorderStyle)
import Brick.Types (Widget, BrickEvent(..), EventM, Next, ViewportType(..), Location(..))
import Brick.Widgets.Border (vBorder, hBorder)
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.BChan (BChan, writeBChan)
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))
import Data.Foldable (toList)
import Data.List (intersperse, foldl')
import Data.List.PointedList (PointedList, _focus, replace, delete, singleton, insert, moveTo, withFocus, find)
import Data.List.PointedList.Circular (next, previous)

data State = State {
    paneZipper :: PaneZipper,
    lastPaneName :: PaneName,
    bottomMenu :: Menu.Menu,
    prompt :: Maybe Prompt.Prompt,
    lastClickedEntry :: Maybe (PaneName, Int, UTCTime),
    editorCommand :: String,
    eventChan :: EChan Tab.Tab,
    searchers :: (Int, Int)
  }
type PaneZipper = PointedList Pane.Pane

-- creation functions
makeState :: FilePath -> String -> EChan Tab.Tab -> Int -> IO State
makeState path editCom eChan tNum = do
  pane <- Pane.make 0 path
  let srcs = (0, if tNum < 1 then 1 else tNum)
      state = State (singleton pane) 0 Menu.make Nothing Nothing editCom eChan srcs
  startSearchers (snd srcs) state

-- rendering functions
drawUi :: State -> [Widget Name]
drawUi state = case prompt state of
  Just pr -> [Prompt.render pr, renderMainUI state]
  _ -> [renderMainUI state]

renderMainUI :: State -> Widget Name
renderMainUI state = vBox [
    renderPanes $ paneZipper state,
    withBorderStyle unicodeBold hBorder,
    vLimit 3 $ Menu.render (bottomMenu state) (currentPane state)
  ]

renderPanes :: PaneZipper -> Widget Name
renderPanes = hBox . intersperse vBorder . map Pane.render . toList . withFocus

-- event handling functions
handleEvent :: State -> BrickEvent Name (ThreadEvent Tab.Tab) -> EventM Name (Next State)
handleEvent state ev = if isSizeEvent ev then handleSize ev state else case prompt state of
  Just pr -> handlePrompt ev pr state
  _ -> handleMain ev state

isSizeEvent :: BrickEvent Name (ThreadEvent Tab.Tab) -> Bool
isSizeEvent (AppEvent ev) = case ev of
  SizeFound _ _ -> True
  SizeNotFound _ -> True
  _ -> False
isSizeEvent _ = False

handleSize :: BrickEvent Name (ThreadEvent Tab.Tab) -> State -> EventM Name (Next State)
handleSize (AppEvent ev) state = case ev of
  SizeFound path val -> manageSearchers . removeSearcher $ notifySize state path (Entry.Known val)
  SizeNotFound path -> manageSearchers . removeSearcher $ notifySize state path Entry.Unknown
  _ -> continue state
handleSize _ state = continue state

handlePrompt :: BrickEvent Name (ThreadEvent Tab.Tab) -> Prompt.Prompt -> State -> EventM Name (Next State)
handlePrompt ev pr state = do
  promptRes <- Prompt.handleEvent ev pr (eventChan state)
  case promptRes of
    Left pr -> updatePrompt pr state --updates the prompt and keeps it up
    Right tab -> updateCurrentPane (Pane.replaceCurrentTab tab) state --updates with the resulting tab and closes the prompt

handleMain :: BrickEvent Name (ThreadEvent Tab.Tab) -> State -> EventM Name (Next State)
handleMain (VtyEvent ev) = case ev of
  EvKey KEsc [] -> halt
  EvKey KBS [] -> updateMenu Menu.Main
  EvKey (KChar 'l') [] -> updateMenu Menu.Selection
  EvKey (KChar 'a') [] -> updateMenu Menu.Tab
  EvKey (KChar 'p') [] -> updateMenu Menu.Pane
  EvKey (KChar 'q') [] -> halt
  EvKey (KChar 'x') [MCtrl] -> updateClipboard Menu.makeCutBoard
  EvKey (KChar 'c') [MCtrl] -> updateClipboard Menu.makeCopyBoard
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
  EvKey KLeft [] -> previousPane
  EvKey KRight [] -> nextPane
  _ -> updateCurrentPane (Pane.handleEvent ev)
handleMain (MouseUp name _ (Location pos)) = case name of
  EntryList {pnName = pName} -> clickedEntry pName (snd pos)
  Label {pnName = pName, labelNum = n} -> updateCurrentPane (Pane.moveToNthTab n) . focusOnPane pName
  Button {keyBind = key, withCtrl = b} -> handleMain . VtyEvent $ EvKey key [MCtrl | b]
  _ -> continue
handleMain _ = continue

-- state-changing functions
updateCurrentPane :: (Pane.Pane -> EventM Name Pane.Pane) -> State -> EventM Name (Next State)
updateCurrentPane func state = do
  newPane <- func $ currentPane state
  manageSearchers $ state {paneZipper = replace newPane $ paneZipper state, prompt = Nothing}

updateMenu :: Menu.MenuType -> State -> EventM Name (Next State)
updateMenu tp st = continue $ st {bottomMenu = Menu.change tp $ bottomMenu st}

updateClipboard :: (Entry.Entry -> Menu.Clipboard) -> State -> EventM Name (Next State)
updateClipboard f st = continue $ case Pane.selectedEntry $ currentPane st of
  (Just entry) -> st {bottomMenu = Menu.changeClipboard (f entry) $ bottomMenu st}
  _ -> st

updatePrompt :: Prompt.Prompt -> State -> EventM Name (Next State)
updatePrompt pr st = continue $ st {prompt=Just pr}

openPrompt :: (Tab.Tab -> PaneName -> Prompt.Prompt) -> State -> EventM Name (Next State)
openPrompt func state = continue $ state {prompt = Just $ func tab pName}
  where
    tab = Pane.currentTab $ currentPane state
    pName = Pane.name $ currentPane state

openPromptWithClip :: (Menu.Clipboard -> Tab.Tab -> PaneName -> Prompt.Prompt) -> State -> EventM Name (Next State)
openPromptWithClip func state = openPrompt (func . Menu.clipboard $ bottomMenu state) state

clickedEntry :: PaneName -> Int -> State -> EventM Name (Next State)
clickedEntry pName row state = do
  currTime <- liftIO getCurrentTime
  let clicked = (pName, row, currTime)
      doubleClick = isDoubleClick (lastClickedEntry state) clicked
  if doubleClick then openEntry $ state {lastClickedEntry = Nothing}
  else updateCurrentPane (Pane.moveTabToRow row) . focusOnPane pName $ state {lastClickedEntry = Just clicked}

isDoubleClick :: Maybe (PaneName, Int, UTCTime) -> (PaneName, Int, UTCTime) -> Bool
isDoubleClick lastClick (nPane, nRow, nTime) = case lastClick of
  Nothing -> False
  Just (pPane, pRow, pTime) -> (pPane == nPane) && (pRow == nRow) && isSmallDiff
    where isSmallDiff = toRational (diffUTCTime nTime pTime) <= toRational 0.2

openEntry :: State -> EventM Name (Next State)
openEntry state = case Pane.selectedEntry $ currentPane state of
  Just Entry.Dir {} -> openDirEntry False state
  Just (Entry.File n p i) -> openFileEntry (Entry.File n p i) state
  _ -> continue state

openFileEntry :: Entry.Entry -> State -> EventM Name (Next State)
openFileEntry fileEntry
  | Entry.isExecutable fileEntry = suspendAndResume . runExternal (Entry.path fileEntry)
  | Entry.isReadable fileEntry = suspendAndResume . runExternalEditor (Entry.path fileEntry)
  | otherwise = continue

runExternalEditor :: FilePath -> State -> IO State
runExternalEditor path s = runExternal (unwords [editorCommand s, path]) s

runExternal :: String -> State -> IO State
runExternal com s = do
  try $ callCommand com :: IO (Either SomeException ())
  putStrLn " "
  putStrLn "Done. Press ENTER to go back to clifm"
  getLine
  return s

openDirEntry :: Bool -> State -> EventM Name (Next State)
openDirEntry inNew = updateCurrentPane (Pane.openDirEntry inNew)

addPane :: State -> EventM Name (Next State)
addPane state = continue $ state {paneZipper = newZipper, lastPaneName = newName}
  where
    newName = 1 + lastPaneName state
    newZipper = insert (Pane.empty newName) $ paneZipper state

closePane :: State -> EventM Name (Next State)
closePane state = continue $ case delete $ paneZipper state of
  Just newZipper -> state {paneZipper = newZipper}
  _ -> state

nextPane :: State -> EventM Name (Next State)
nextPane state = continue $ state {paneZipper = next $ paneZipper state}

previousPane :: State -> EventM Name (Next State)
previousPane state = continue $ state {paneZipper = previous $ paneZipper state}

-- directory entry size / threaded searchers / value update
removeSearcher :: State -> State
removeSearcher state = state {searchers = (running - 1, maxNum)}
  where (running, maxNum) = searchers state

manageSearchers :: State -> EventM Name (Next State)
manageSearchers state
  | running >= maxNum = continue state
  | otherwise = continue =<< liftIO (startSearchers (maxNum - running) state)
  where (running, maxNum) = searchers state

startSearchers :: Int -> State -> IO State --TODO: lens could really improve this
startSearchers n state = do
  let waiting = concatMap Pane.waitingEntries . toList $ paneZipper state
      toStart = map Entry.path . take n . Set.toList $ Set.fromList waiting
      (running, maxNum) = searchers state
      countState = state {searchers = (running + length toStart, maxNum)}
  mapM_ (startSearcher (eventChan state)) toStart
  return $ foldl' (\st path -> notifySize st path Entry.Calculating) countState toStart 

startSearcher :: EChan Tab.Tab -> FilePath -> IO ThreadId
startSearcher eChan path = forkFinally (Entry.getDirSize path) (reportSearch path eChan)

reportSearch :: FilePath -> EChan Tab.Tab -> Either SomeException Integer -> IO ()
reportSearch path eChan = writeBChan eChan . either (const (SizeNotFound path)) (SizeFound path)

notifySize :: State -> FilePath -> Entry.Size -> State
notifySize state path size = state {paneZipper = Pane.notifySize path size <$> paneZipper state}

-- pane and paneZipper utility functions
currentPane :: State -> Pane.Pane
currentPane = _focus . paneZipper

focusOnPane :: PaneName -> State -> State
focusOnPane pName state = case find (Pane.empty pName) $ paneZipper state of
  Just newZipper -> state {paneZipper = newZipper}
  _ -> state
