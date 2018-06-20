module Widgets.Menu where
import Commons
import qualified Widgets.Pane as Pane
import qualified Widgets.Tab as Tab
import qualified Widgets.Entry as Entry

import Control.Lens
import Data.Char (toUpper)
import System.FilePath (takeFileName)
import Brick.Widgets.Core ((<+>), str, hLimit, hBox, clickable)
import Brick.Types (Widget)
import Brick.Widgets.Border (borderWithLabel, border)
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))

data Menu = Menu {_clipboard :: Clipboard, _menuType :: MenuType}
data MenuType = Main | Selection | Tab | Pane
data Clipboard = CopyBoard {_fromEntry :: Entry.Entry} | CutBoard {_fromEntry :: Entry.Entry} | EmptyBoard

instance Show Clipboard where
  show EmptyBoard = " -empty- "
  show board = takeFileName $ board ^. fromEntry . Entry.path

-- lenses
clipboard :: Lens' Menu Clipboard
clipboard = lens _clipboard (\menu x -> menu {_clipboard = x})

menuType :: Lens' Menu MenuType
menuType = lens _menuType (\menu x -> menu {_menuType = x})

fromEntry :: Traversal' Clipboard Entry.Entry
fromEntry = filtered (not . isEmpty) . lens _fromEntry (\board x -> board {_fromEntry = x})

-- creation
make :: Menu
make = Menu {_clipboard = EmptyBoard, _menuType = Main}

-- rendering
render :: Menu -> Pane.Pane -> Widget Name
render m = hBox . (views clipboard renderClipboard m :) . views menuType renderButtons m

renderButtons :: MenuType -> Pane.Pane -> [Widget Name]
renderButtons tp pane = map renderButton $ case tp of
  Main -> mainButtons
  Selection -> backButton : maybe [] selectionButtons (Pane.selectedEntry pane)
  Tab -> backButton : views Pane.currentTab tabButtons pane
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

selectionButtons :: Entry.Entry -> [(Widget Name, Maybe String, Name)]
selectionButtons e
  | Entry.isDir e = anySelectionButtons ++ [(keybindStr "o" <+> str "pen in new tab", ctrlText 'o', Button {keyBind = KChar 'o', withCtrl = True})]
  | otherwise = anySelectionButtons

anySelectionButtons :: [(Widget Name, Maybe String, Name)]
anySelectionButtons = [
    (str "cut", ctrlText 'x', Button {keyBind = KChar 'x', withCtrl = True}),
    (str "copy", ctrlText 'c', Button {keyBind = KChar 'c', withCtrl = True}),
    (keybindStr "r" <+> str "ename", ctrlText 'r', Button {keyBind = KChar 'r', withCtrl = True}),
    (keybindStr "d" <+> str "elete", ctrlText 'd', Button {keyBind = KChar 'd', withCtrl = True}),
    (keybindStr "s" <+> str "how info", Nothing, Button {keyBind = KChar 's', withCtrl = False})
  ]

tabButtons :: Tab.Tab -> [(Widget Name, Maybe String, Name)]
tabButtons tab
  | Tab.isDir tab = dirTabButtons ++ entryTabButtons orderType ++ anyTabButtons
  | Tab.isSearch tab = entryTabButtons orderType ++ anyTabButtons
  | otherwise = anyTabButtons
  where (Just orderType) = preview Tab.orderType tab

dirTabButtons :: [(Widget Name, Maybe String, Name)]
dirTabButtons = [
    (str "paste", ctrlText 'v', Button {keyBind = KChar 'v', withCtrl = True}),
    (keybindStr "s" <+> str "earch", ctrlText 's', Button {keyBind = KChar 's', withCtrl = True}),
    (keybindStr "m" <+> str "ake dir", Nothing, Button {keyBind = KChar 'm', withCtrl = False}),
    (keybindStr "t" <+> str "ouch file", Nothing, Button {keyBind = KChar 't', withCtrl = False})
  ]

entryTabButtons :: Tab.OrderType -> [(Widget Name, Maybe String, Name)]
entryTabButtons orderType = [
    (keybindStr "r" <+> str "efresh", Nothing, Button {keyBind = KChar 'r', withCtrl = False}),
    (keybindStr "o" <+> str ("rder by " ++ show (Tab.nextOrderType orderType)), Nothing, Button {keyBind = KChar 'o', withCtrl = False}),
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

-- utility
isEmpty :: Clipboard -> Bool
isEmpty EmptyBoard = True
isEmpty _ = False

isCopy :: Clipboard -> Bool
isCopy CopyBoard {} = True
isCopy _ = False

isCut :: Clipboard -> Bool
isCut CutBoard {} = True
isCut _ = False