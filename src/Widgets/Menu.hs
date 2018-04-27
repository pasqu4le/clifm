module Widgets.Menu where
import Commons
import qualified Widgets.Pane as Pane
import qualified Widgets.Tab as Tab
import qualified Widgets.Entry as Entry

import Data.Char (toUpper)
import System.FilePath (takeFileName)
import Brick.Widgets.Core ((<+>), str, hLimit, hBox, clickable)
import Brick.Types (Widget)
import Brick.Widgets.Border (borderWithLabel, border)
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))

data Menu = Menu {clipboard :: Clipboard, menuType :: MenuType}
data MenuType = Main | Selection | Tab | Pane
data Clipboard = CopyBoard {fromEntry :: Entry.Entry} | CutBoard {fromEntry :: Entry.Entry} | EmptyBoard

instance Show Clipboard where
  show EmptyBoard = " -empty- "
  show board = takeFileName . Entry.path $ fromEntry board

-- creation functions
make :: Menu
make = Menu {clipboard = EmptyBoard, menuType = Main}

makeCopyBoard :: Entry.Entry -> Clipboard
makeCopyBoard = CopyBoard

makeCutBoard :: Entry.Entry -> Clipboard
makeCutBoard = CutBoard

-- rendering functions
render :: Menu -> Pane.Pane -> Widget Name
render m = hBox . (renderClipboard (clipboard m) :) . renderButtons (menuType m)

renderButtons :: MenuType -> Pane.Pane -> [Widget Name]
renderButtons tp pane = map renderButton $ case tp of
  Main -> mainButtons
  Selection -> (backButton :) . selectionButtons $ Pane.selectedEntry pane
  Tab -> (backButton :) . tabButtons $ Pane.currentTab pane
  Pane -> backButton : paneButtons

renderButton :: (Widget Name, Maybe String, Name) -> Widget Name
renderButton (bContent, bLabel, bName) = clickable bName $ case bLabel of
  Just txt -> borderWithLabel (str txt) bContent
  Nothing -> border bContent

mainButtons :: [(Widget Name, Maybe String, Name)]
mainButtons = [
    (str "se" <+> keybindStr "l" <+> str "ection menu", Nothing, Button {keyBind = KChar 'l', withCtrl = False}),
    (str "t" <+> keybindStr "a" <+> str "b menu", Nothing, Button {keyBind = KChar 'a', withCtrl = False}),
    (keybindStr "p" <+> str "ane menu", Nothing, Button {keyBind = KChar 'p', withCtrl = False}),
    (keybindStr "q" <+> str "uit", Nothing, Button {keyBind = KChar 'q', withCtrl = False})
  ]

selectionButtons :: Maybe Entry.Entry -> [(Widget Name, Maybe String, Name)]
selectionButtons e = case e of
  Just Entry.File {} -> anySelectionButtons
  Just Entry.Dir {} -> anySelectionButtons ++ [(keybindStr "o" <+> str "pen in new tab", ctrlText 'o', Button {keyBind = KChar 'o', withCtrl = True})]
  _ -> []

anySelectionButtons :: [(Widget Name, Maybe String, Name)]
anySelectionButtons = [
    (str "cut", ctrlText 'x', Button {keyBind = KChar 'x', withCtrl = True}),
    (str "copy", ctrlText 'c', Button {keyBind = KChar 'c', withCtrl = True}),
    (keybindStr "r" <+> str "ename", ctrlText 'r', Button {keyBind = KChar 'r', withCtrl = True}),
    (keybindStr "d" <+> str "elete", ctrlText 'd', Button {keyBind = KChar 'd', withCtrl = True}),
    (keybindStr "s" <+> str "how info", Nothing, Button {keyBind = KChar 's', withCtrl = False})
  ]

tabButtons :: Tab.Tab -> [(Widget Name, Maybe String, Name)]
tabButtons tab = case tab of
  Tab.Dir {Tab.entryOrder = order} -> dirTabButtons ++ entryTabButtons order ++ anyTabButtons
  Tab.Search {Tab.entryOrder = order} -> entryTabButtons order ++ anyTabButtons
  _ -> anyTabButtons

dirTabButtons :: [(Widget Name, Maybe String, Name)]
dirTabButtons = [
    (str "paste", ctrlText 'v', Button {keyBind = KChar 'v', withCtrl = True}),
    (keybindStr "s" <+> str "earch", ctrlText 's', Button {keyBind = KChar 's', withCtrl = True}),
    (keybindStr "m" <+> str "ake dir", Nothing, Button {keyBind = KChar 'm', withCtrl = False}),
    (keybindStr "t" <+> str "ouch file", Nothing, Button {keyBind = KChar 't', withCtrl = False})
  ]

entryTabButtons :: Tab.EntryOrder -> [(Widget Name, Maybe String, Name)]
entryTabButtons order = [
    (keybindStr "r" <+> str "efresh", Nothing, Button {keyBind = KChar 'r', withCtrl = False}),
    (keybindStr "o" <+> str ("rder by " ++ (show . Tab.nextOrderType $ Tab.orderType order)), Nothing, Button {keyBind = KChar 'o', withCtrl = False}),
    (keybindStr "i" <+> str "nvert order", Nothing, Button {keyBind = KChar 'i', withCtrl = False})
  ]

anyTabButtons :: [(Widget Name, Maybe String, Name)]
anyTabButtons = [
    (keybindStr "g" <+> str "o to", Nothing, Button {keyBind = KChar 'g', withCtrl = False}),
    (keybindStr "e" <+> str "mpty tab", Nothing, Button {keyBind = KChar 'e', withCtrl = False}),
    (keybindStr "k" <+> str "ill tab", Nothing, Button {keyBind = KChar 'k', withCtrl = False})
  ]

paneButtons :: [(Widget Name, Maybe String, Name)]
paneButtons = [
    (keybindStr "e" <+> str "mpty pane", ctrlText 'e', Button {keyBind = KChar 'e', withCtrl = True}),
    (keybindStr "k" <+> str "ill pane", ctrlText 'k', Button {keyBind = KChar 'k', withCtrl = True})
  ]

ctrlText :: Char -> Maybe String
ctrlText c = Just $ "C-" ++ [toUpper c]

backButton :: (Widget Name, Maybe String, Name)
backButton = (str "<_", Nothing, Button {keyBind = KBS, withCtrl = False})

renderClipboard :: Clipboard -> Widget Name
renderClipboard = hLimit 24 . borderWithLabel (str "clipboard") . str . show

-- state changing functions
change :: MenuType -> Menu -> Menu
change tp menu = menu {menuType = tp}

changeClipboard :: Clipboard -> Menu -> Menu
changeClipboard cb menu = menu {clipboard = cb}
