module Widgets.Manager where
import Commons
import qualified Widgets.Pane as Pane
import qualified Widgets.Tab as Tab
import qualified Widgets.Entry as Entry
import qualified Widgets.Menu as Menu
import qualified Widgets.Prompt as Prompt

import System.Process (callCommand)
import Control.Exception (try, SomeException)
import Brick.Main (continue, halt, suspendAndResume)
import Brick.Widgets.Core ((<+>), str, hBox, vBox, vLimit, withBorderStyle)
import Brick.Types (Widget, BrickEvent(..), EventM, Next, ViewportType(..), Location(..))
import Brick.Widgets.Border (vBorder, hBorder)
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.BChan (BChan)
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.PointedList (PointedList, _focus, replace, delete, singleton, insert, moveTo, withFocus, find)
import Data.List.PointedList.Circular (next, previous)

data State = State {paneZipper :: PaneZipper,
    lastPaneName :: PaneName,
    bottomMenu :: Menu.Menu,
    prompt :: Maybe Prompt.Prompt,
    editorCommand :: String,
    eventChan :: BChan (ThreadEvent Tab.Tab)
  }
type PaneZipper = PointedList Pane.Pane

-- creation functions
makeState :: FilePath -> String -> BChan (ThreadEvent Tab.Tab) -> IO State
makeState path editCom eChan = do
  pane <- Pane.make 0 path
  return $ State (singleton pane) 0 Menu.make Nothing editCom eChan

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
handleEvent state event = case prompt state of
  Just pr -> handlePrompt event pr state
  _ -> handleMain event state

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
  EntryList {pnName = pName} -> updateCurrentPane (Pane.moveTabToRow $ snd pos) . focusOnPane pName
  Label {pnName = pName, labelNum = n} -> updateCurrentPane (Pane.updateTabZipper (Pane.moveToNth n)) . focusOnPane pName
  Button {keyBind = key, withCtrl = b} -> handleMain . VtyEvent $ EvKey key [MCtrl | b]
  _ -> continue
handleMain _ = continue

-- state-changing functions
updateCurrentPane :: (Pane.Pane -> EventM Name Pane.Pane) -> State -> EventM Name (Next State)
updateCurrentPane func state = do
  newPane <- func $ currentPane state
  continue $ state {paneZipper = replace newPane $ paneZipper state, prompt = Nothing}

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

-- pane and paneZipper utility functions
currentPane :: State -> Pane.Pane
currentPane = _focus . paneZipper

focusOnPane :: PaneName -> State -> State
focusOnPane pName state = case find (Pane.empty pName) $ paneZipper state of
  Just newZipper -> state {paneZipper = newZipper}
  _ -> state
