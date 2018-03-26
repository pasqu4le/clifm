module Widgets.Manager where
import Types
import ListZipper
import Widgets.Tab
import Widgets.Clipboard
import Widgets.Prompt

import Control.Monad.IO.Class (liftIO)
import Brick.Main (continue, halt)
import Brick.Widgets.Core ((<+>), str, hBox, vBox, vLimit, viewport, withBorderStyle, withDefAttr, clickable)
import Brick.Types (Widget, BrickEvent(..), EventM, Next, ViewportType(..), Location(..))
import Brick.Widgets.Border (border, hBorder)
import Brick.Widgets.Border.Style (unicodeBold)
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))

data State = State {tabZipper :: TabZipper, clipboard :: Clipboard, prompt :: Maybe Prompt}
type TabZipper = Zipper Tab

-- creation functions
makeState :: FilePath -> IO State
makeState path = (\zp -> State zp EmptyBoard Nothing) <$> makeTabZipper path

makeTabZipper :: FilePath -> IO TabZipper
makeTabZipper path = (\tab -> Zipper tab [] []) <$> makeDirTab path

-- rendering functions
drawUi :: State -> [Widget Name]
drawUi state = case prompt state of
  Just pr -> [renderPrompt pr, renderMainUI state]
  _ -> [renderMainUI state]

renderMainUI :: State -> Widget Name
renderMainUI state = vBox [labels, topSep, content, botSep,  menu]
  where
    zipper = tabZipper state
    labels = vLimit 2 . viewport LScroll Horizontal $ renderLabels zipper
    topSep = renderPathSeparator $ current zipper
    content = clickable EList . renderContent $ current zipper
    botSep = withBorderStyle unicodeBold hBorder
    menu = vLimit 3 $ renderMenu state

renderLabels :: TabZipper -> Widget Name
renderLabels zipper = hBox . map clickableLabel $ zip labels [0..]
  where labels = mapWithCurrent renderLabel zipper

clickableLabel :: (Widget Name, Int) -> Widget Name
clickableLabel (l, n) = clickable (LNum n) l

renderMenu :: State -> Widget Name
renderMenu (State z c _) = hBox (renderClipboard c : (renderButtons . current) z)

renderButtons :: Tab -> [Widget Name]
renderButtons tab = map renderButton $ tabButtons tab ++ indipendentButtons

renderButton :: (Widget Name, Char) -> Widget Name
renderButton (s, c) = clickable (BVal c) $ border s

indipendentButtons :: [(Widget Name, Char)]
indipendentButtons = [
    (withDefAttr keybindAttr (str "n") <+> str "ew tab", 'n'),
    (withDefAttr keybindAttr (str "g") <+> str "o to", 'g'),
    (withDefAttr keybindAttr (str "k") <+> str "ill tab", 'k'),
    (withDefAttr keybindAttr (str "q") <+> str "uit", 'q')
  ]

-- event handling functions
handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent state event = case prompt state of
  Just pr -> handlePrompt event pr state
  _ -> handleMain event state

handlePrompt :: BrickEvent Name e -> Prompt -> State -> EventM Name (Next State)
handlePrompt (VtyEvent ev) pr state = do
  promptRes <- handlePromptEvent ev pr
  case promptRes of
    Left pr -> updatePrompt pr state --updates the prompt and keeps it up
    Right tab -> updateZipper (replace tab) state --replaces with the resulting tab and closes the prompt
handlePrompt _ _ state = continue state

handleMain :: BrickEvent Name e -> State -> EventM Name (Next State)
handleMain (VtyEvent ev) = case ev of
  EvKey KEsc [] -> halt
  EvKey (KChar 'q') [MCtrl] -> halt
  EvKey (KChar 'x') [MCtrl] -> updateClipboard makeCutBoard
  EvKey (KChar 'c') [MCtrl] -> updateClipboard makeCopyBoard
  EvKey (KChar 'v') [MCtrl] -> openPromptWithClip makePastePrompt
  EvKey (KChar 'r') [MCtrl] -> openPrompt makeRenamePrompt
  EvKey (KChar 'd') [MCtrl] -> openPrompt makeDeletePrompt
  EvKey (KChar 'a') [MCtrl] -> openPrompt makeMkdirPrompt
  EvKey (KChar 't') [MCtrl] -> openPrompt makeTouchPrompt
  EvKey (KChar 'g') [MCtrl] -> openPrompt makeGoToPrompt
  EvKey (KChar 'n') [MCtrl] -> updateZipper (insertRightAndFocus makeEmptyTab)
  EvKey (KChar 'o') [MCtrl] -> updateZipperEv (openTabEntry True)
  EvKey (KChar 'l') [MCtrl] -> updateZipperEv reloadCurrentTab
  EvKey (KChar 'k') [MCtrl] -> updateZipper (remove makeEmptyTab)
  EvKey (KChar '\t') [] -> updateZipper nextCiclical
  EvKey KBackTab [] -> updateZipper prevCiclical
  EvKey KLeft [MCtrl] -> updateZipper swapPrevCiclical
  EvKey KRight [MCtrl] -> updateZipper swapNextCiclical
  EvKey KEnter [] -> updateZipperEv (openTabEntry False)
  _ -> updateZipperEv (updateCurrentTab ev)
handleMain (MouseUp name _ (Location pos)) = case name of
  EList -> updateZipper (moveTabToRow $ snd pos)
  (LNum n) -> updateZipper (moveToNth n)
  (BVal c) -> handleMain (VtyEvent (EvKey (KChar c) [MCtrl]))
  _ -> continue
handleMain _ = continue

-- state-changing functions
updateZipper :: (TabZipper -> TabZipper) -> State -> EventM Name (Next State)
updateZipper f (State zipper clbrd _) = continue (State (f zipper) clbrd Nothing)

updateClipboard :: (Entry -> Clipboard) -> State -> EventM Name (Next State)
updateClipboard f (State zp cl _) = continue $ case selectedEntry $ current zp of
  (Just entry) -> State zp (f entry) Nothing
  _ -> State zp cl Nothing

updatePrompt :: Prompt -> State -> EventM Name (Next State)
updatePrompt pr (State z c _) = continue . State z c $ Just pr

openPrompt :: (Tab -> Prompt) -> State -> EventM Name (Next State)
openPrompt f (State z c _) = continue . State z c . Just . f $ current z

openPromptWithClip :: (Clipboard -> Tab -> Prompt) -> State -> EventM Name (Next State)
openPromptWithClip f (State z c _) = continue . State z c . Just . f c $ current z

updateZipperEv :: (Tab -> EventM Name (TabZipper -> TabZipper)) -> State -> EventM Name (Next State)
updateZipperEv inputFunc s = do
  func <- inputFunc . current $ tabZipper s
  updateZipper func s

openTabEntry :: Bool -> Tab -> EventM Name (TabZipper -> TabZipper)
openTabEntry inNew tab = do
  tabRes <- liftIO $ openEntry tab
  return $ case tabRes of
    Just t -> (if inNew then insertRight else replace) t
    _ -> id

reloadCurrentTab :: Tab -> EventM Name (TabZipper -> TabZipper)
reloadCurrentTab tab = replace <$> (liftIO $ reload tab)

updateCurrentTab :: Event -> Tab -> EventM Name (TabZipper -> TabZipper)
updateCurrentTab ev tab = replace <$> handleTabEvent ev tab

-- tab and tabZipper utility functions
moveTabToRow :: Int -> TabZipper -> TabZipper
moveTabToRow row zipper = replace (moveToRow row $ current zipper) zipper
