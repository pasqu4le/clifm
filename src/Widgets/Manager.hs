module Widgets.Manager where
import Types
import Widgets.Pane
import Widgets.Tab
import Widgets.Menu
import Widgets.Prompt

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
    bottomMenu :: Menu,
    prompt :: Maybe Prompt,
    editorCommand :: String,
    eventChan :: BChan (ThreadEvent Tab)
  }
type PaneZipper = PointedList Pane

-- creation functions
makeState :: FilePath -> String -> BChan (ThreadEvent Tab) -> IO State
makeState path editCom eChan = do
  pane <- makePane 0 path
  return $ State (singleton pane) 0 makeMenu Nothing editCom eChan

-- rendering functions
drawUi :: State -> [Widget Name]
drawUi state = case prompt state of
  Just pr -> [renderPrompt pr, renderMainUI state]
  _ -> [renderMainUI state]

renderMainUI :: State -> Widget Name
renderMainUI state = vBox [panes, botSep, menu]
  where
    panes = renderPanes $ paneZipper state
    botSep = withBorderStyle unicodeBold hBorder
    menu = vLimit 3 $ renderMenu (bottomMenu state) (currentPane state)

renderPanes :: PaneZipper -> Widget Name
renderPanes = hBox . intersperse vBorder . map renderPane . toList . withFocus

-- event handling functions
handleEvent :: State -> BrickEvent Name (ThreadEvent Tab) -> EventM Name (Next State)
handleEvent state event = case prompt state of
  Just pr -> handlePrompt event pr state
  _ -> handleMain event state

handlePrompt :: BrickEvent Name (ThreadEvent Tab) -> Prompt -> State -> EventM Name (Next State)
handlePrompt ev pr state = do
  promptRes <- handlePromptEvent ev pr (eventChan state)
  case promptRes of
    Left pr -> updatePrompt pr state --updates the prompt and keeps it up
    Right tab -> updateCurrentPane (updateTabZipper (replace tab)) state --updates with the resulting tab and closes the prompt

handleMain :: BrickEvent Name (ThreadEvent Tab) -> State -> EventM Name (Next State)
handleMain (VtyEvent ev) = case ev of
  EvKey KEsc [] -> halt
  EvKey KBS [] -> updateMenu MainMenu
  EvKey (KChar 'l') [] -> updateMenu SelectionMenu
  EvKey (KChar 'a') [] -> updateMenu TabMenu
  EvKey (KChar 'p') [] -> updateMenu PaneMenu
  EvKey (KChar 'q') [] -> halt
  EvKey (KChar 'x') [MCtrl] -> updateClipboard makeCutBoard
  EvKey (KChar 'c') [MCtrl] -> updateClipboard makeCopyBoard
  EvKey (KChar 'v') [MCtrl] -> openPromptWithClip makePastePrompt
  EvKey (KChar 'r') [MCtrl] -> openPrompt makeRenamePrompt
  EvKey (KChar 'd') [MCtrl] -> openPrompt makeDeletePrompt
  EvKey (KChar 'o') [MCtrl] -> openTabDir True
  EvKey (KChar 's') [MCtrl] -> openPrompt makeSearchPrompt
  EvKey (KChar 's') [] -> openPrompt makeDisplayInfoPrompt
  EvKey (KChar 'm') [] -> openPrompt makeMkdirPrompt
  EvKey (KChar 't') [] -> openPrompt makeTouchPrompt
  EvKey (KChar 'g') [] -> openPrompt makeGoToPrompt
  EvKey KEnter [] -> openTabEntry
  EvKey (KChar 'e') [MCtrl] -> addPane
  EvKey (KChar 'k') [MCtrl] -> closePane
  EvKey KLeft [] -> previousPane
  EvKey KRight [] -> nextPane
  _ -> updateCurrentPane (handlePaneEvent ev)
handleMain (MouseUp name _ (Location pos)) = case name of
  EntryList {pnName = pName} -> updateCurrentPane (moveTabToRow $ snd pos) . focusOnPane pName
  Label {pnName = pName, labelNum = n} -> updateCurrentPane (updateTabZipper (moveToNth n)) . focusOnPane pName
  Button {keyBind = key, withCtrl = b} -> handleMain . VtyEvent $ EvKey key [MCtrl | b]
  _ -> continue
handleMain _ = continue

-- state-changing functions
updateCurrentPane :: (Pane -> EventM Name Pane) -> State -> EventM Name (Next State)
updateCurrentPane func state = do
  newPane <- func $ currentPane state
  continue $ state {paneZipper = replace newPane $ paneZipper state, prompt = Nothing}

updateMenu :: MenuType -> State -> EventM Name (Next State)
updateMenu tp st = continue $ st {bottomMenu = changeMenu tp $ bottomMenu st}

updateClipboard :: (Entry -> Clipboard) -> State -> EventM Name (Next State)
updateClipboard f st = continue $ case selectedEntry . currentTab $ currentPane st of
  (Just entry) -> st {bottomMenu = changeClipboard (f entry) $ bottomMenu st}
  _ -> st

updatePrompt :: Prompt -> State -> EventM Name (Next State)
updatePrompt pr st = continue $ st {prompt=Just pr}

openPrompt :: (Tab -> PaneName -> Prompt) -> State -> EventM Name (Next State)
openPrompt func state = continue $ state {prompt = Just $ func tab pName}
  where
    tab = currentTab $ currentPane state
    pName = paneName $ currentPane state

openPromptWithClip :: (Clipboard -> Tab -> PaneName -> Prompt) -> State -> EventM Name (Next State)
openPromptWithClip func state = openPrompt (func . clipboard $ bottomMenu state) state

openTabEntry :: State -> EventM Name (Next State)
openTabEntry state = case selectedEntry . currentTab $ currentPane state of
  Just DirEntry {} -> openTabDir False state
  Just (FileEntry n p i) -> openTabFile (FileEntry n p i) state
  _ -> continue state

openTabFile :: Entry -> State -> EventM Name (Next State)
openTabFile fileEntry
  | isExecutable fileEntry = suspendAndResume . runExternal (entryPath fileEntry)
  | isReadable fileEntry = suspendAndResume . runExternalEditor (entryPath fileEntry)
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

openTabDir :: Bool -> State -> EventM Name (Next State)
openTabDir inNew = updateCurrentPane (openSelectedDir inNew)

addPane :: State -> EventM Name (Next State)
addPane state = continue $ state {paneZipper = newZipper, lastPaneName = newName}
  where
    newName = 1 + lastPaneName state
    newZipper = insert (makeEmptyPane newName) $ paneZipper state

closePane :: State -> EventM Name (Next State)
closePane state = continue $ case delete $ paneZipper state of
  Just newZipper -> state {paneZipper = newZipper}
  _ -> state

nextPane :: State -> EventM Name (Next State)
nextPane state = continue $ state {paneZipper = next $ paneZipper state}

previousPane :: State -> EventM Name (Next State)
previousPane state = continue $ state {paneZipper = previous $ paneZipper state}

-- pane and paneZipper utility functions
currentPane :: State -> Pane
currentPane = _focus . paneZipper

focusOnPane :: PaneName -> State -> State
focusOnPane pName state = case find (makeEmptyPane pName) $ paneZipper state of
  Just newZipper -> state {paneZipper = newZipper}
  _ -> state
