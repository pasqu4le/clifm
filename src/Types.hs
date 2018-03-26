module Types where

import Data.Monoid ((<>))
import Brick.AttrMap (AttrName, AttrMap, attrName, attrMap)
import Graphics.Vty (defAttr, withStyle, underline, black, yellow, white, blue, red)
import Brick.Util (on, fg, bg)
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (listSelectedFocusedAttr)

-- names
data Name = BVal Char | LScroll | LNum Int | PEdit | EList deriving (Ord, Show, Eq)

-- attributes
attributes :: AttrMap
attributes = attrMap defAttr [
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
