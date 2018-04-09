module Types where

import Data.Monoid ((<>))
import Brick.Widgets.Core (withDefAttr, str)
import Brick.Types (Widget)
import Brick.Themes (Theme, newTheme)
import Brick.AttrMap (AttrName, AttrMap, attrName, attrMap)
import Graphics.Vty (defAttr, withStyle, underline, black, yellow, white, blue, red)
import Brick.Util (on, fg, bg)
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (listSelectedFocusedAttr)

-- names and events
data Name = BVal Char Bool | LScroll | LNum Int | PEdit | EList deriving (Ord, Show, Eq)
data ThreadEvent a = ThreadClosed | ThreadSuccess a | ThreadError String

-- attributes and themes
defaultTheme :: Theme
defaultTheme = newTheme (white `on` black) [
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
