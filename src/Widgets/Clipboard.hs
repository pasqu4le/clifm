module Widgets.Clipboard where
import Types
import Widgets.Tab

import System.FilePath (takeFileName)
import Brick.Widgets.Core (str, hLimit)
import Brick.Types (Widget)
import Brick.Widgets.Border (borderWithLabel)

data Clipboard = CopyBoard {copyFrom :: Entry} | CutBoard {cutFrom :: Entry} | EmptyBoard

instance Show Clipboard where
  show (CopyBoard entry) = takeFileName $ entryPath entry
  show (CutBoard entry) = takeFileName $ entryPath entry
  show _ = " -empty- "

-- creation functions
makeCopyBoard :: Entry -> Clipboard
makeCopyBoard = CopyBoard

makeCutBoard :: Entry -> Clipboard
makeCutBoard = CutBoard

-- rendering functions
renderClipboard :: Clipboard -> Widget Name
renderClipboard = hLimit 24 . borderWithLabel (str "clipboard") . str . show
