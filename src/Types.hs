module Types where

import Data.Monoid ((<>))
import Brick.Widgets.Core (withDefAttr, str)
import Brick.Types (Widget)
import Brick.Themes (Theme, newTheme)
import Brick.AttrMap (AttrName, AttrMap, attrName, attrMap)
import Graphics.Vty (defAttr, withStyle, underline, black, yellow, white, blue, red)
import Brick.Util (on, fg, bg)
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (listSelectedFocusedAttr, listSelectedAttr)

-- data definitions
data Name = Button {charBind :: Char, withCtrl :: Bool} |
  LabelsRow {pnName :: PaneName} |
  Label {pnName :: PaneName, labelNum :: Int} |
  PromptEditor |
  EntryList {pnName :: PaneName} deriving (Ord, Show, Eq)
data ThreadEvent a = ThreadClosed | ThreadSuccess a | ThreadError String
type PaneName = Int

-- attributes and themes
defaultTheme :: Theme
defaultTheme = newTheme (white `on` black) [
    (listSelectedAttr, fg yellow),
    (listSelectedFocusedAttr, black `on` yellow),
    (keybindAttr, fg white `withStyle` underline),
    (promptAttr, bg blue),
    (errorAttr, bg red),
    (editFocusedAttr, black `on` yellow),
    (disclaimerAttr, black `on` white)
  ]

keybindAttr :: AttrName
keybindAttr = attrName "keybind"

promptAttr :: AttrName
promptAttr = attrName "prompt"

errorAttr :: AttrName
errorAttr = attrName "error"

disclaimerAttr :: AttrName
disclaimerAttr = attrName "disclaimer"

-- utility functions
keybindStr :: String -> Widget Name
keybindStr = withDefAttr keybindAttr . str
