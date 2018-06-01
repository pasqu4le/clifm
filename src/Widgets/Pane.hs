module Widgets.Pane where
import Commons
import qualified Widgets.Tab as Tab
import qualified Widgets.Entry as Entry

import Control.Monad.IO.Class (liftIO)
import Brick.Widgets.Core (hBox, vBox, vLimit, viewport, clickable)
import Brick.Types (Widget, BrickEvent(..), EventM, ViewportType(..))
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))
import Data.Foldable (toList)
import Data.List.PointedList (PointedList, _focus, replace, delete, singleton, insert, insertLeft, moveTo, withFocus, atStart, atEnd)
import Data.List.PointedList.Circular (next, previous)

data Pane = Pane {name :: PaneName, tabZipper :: TabZipper}
type TabZipper = PointedList Tab.Tab

instance Eq Pane where
  Pane {name = p1} == Pane {name = p2} = p1 == p2

-- creation functions
empty :: PaneName -> Pane
empty pName = Pane pName . singleton $ Tab.empty

make :: PaneName -> FilePath -> IO Pane
make pName path = Pane pName . singleton <$> Tab.makeDirTab pName path

-- rendering functions
render :: (Pane, Bool) -> Widget Name
render (pane, hasFocus) = let pName = name pane in vBox [
    vLimit 2 . viewport LabelsRow {pnName = pName} Horizontal . renderLabels pName $ tabZipper pane,
    Tab.renderSeparator $ currentTab pane,
    clickable EntryList {pnName = pName} . Tab.renderContent hasFocus $ currentTab pane
  ]

renderLabels :: PaneName -> TabZipper -> Widget Name
renderLabels pName zipper = hBox . map (clickableLabel pName) $ zip labels [0..]
  where labels = map Tab.renderLabel . toList $ withFocus zipper

clickableLabel :: PaneName -> (Widget Name, Int) -> Widget Name
clickableLabel pName (l, n) = clickable Label {pnName = pName, labelNum = n} l

-- event handling functions
handleEvent :: Event -> Pane -> EventM Name Pane
handleEvent event = case event of
  EvKey (KChar 'k') [] -> updateTabZipper removeTab
  EvKey (KChar 'e') [] -> updateTabZipper (insert Tab.empty)
  EvKey (KChar 'r') [] -> reloadCurrentTab
  EvKey (KChar '\t') [] -> updateTabZipper next
  EvKey KBackTab [] -> updateTabZipper previous
  EvKey KLeft [MCtrl] -> updateTabZipper swapPrev
  EvKey KRight [MCtrl] -> updateTabZipper swapNext
  _ -> updateCurrentTab event

-- state-changing functions
updateTabZipper :: (TabZipper -> TabZipper) -> Pane -> EventM Name Pane
updateTabZipper func pane = return $ pane {tabZipper = func $ tabZipper pane}

reloadCurrentTab :: Pane -> EventM Name Pane
reloadCurrentTab pane = do
  reloaded <- liftIO . Tab.reload (name pane) $ currentTab pane
  updateTabZipper (replace reloaded) pane

updateCurrentTab :: Event -> Pane -> EventM Name Pane
updateCurrentTab event pane = do
  updated <- Tab.handleEvent event $ currentTab pane
  updateTabZipper (replace updated) pane

openDirEntry :: Bool -> Pane -> EventM Name Pane
openDirEntry inNew pane = case selectedEntry pane of
  Just Entry.Dir {Entry.path = path} -> do
    loaded <- liftIO $ Tab.makeDirTab (name pane) path
    updateTabZipper (if inNew then previous . insert loaded else replace loaded) pane
  _ -> return pane

replaceCurrentTab :: Tab.Tab -> Pane -> EventM Name Pane
replaceCurrentTab tab = updateTabZipper (replace tab)

moveToNthTab :: Int -> Pane -> EventM Name Pane
moveToNthTab n = updateTabZipper (moveToNth n)

-- tab and tabZipper utility functions
selectedEntry :: Pane -> Maybe Entry.Entry
selectedEntry = Tab.selectedEntry . currentTab

moveTabToRow :: Int -> Pane -> EventM Name Pane
moveTabToRow row pane = updateTabZipper (replace (Tab.moveToRow row $ currentTab pane)) pane

currentTab :: Pane -> Tab.Tab
currentTab = _focus . tabZipper

removeTab :: TabZipper -> TabZipper
removeTab zipper = case delete zipper of
  Just newZipper -> newZipper
  _ -> singleton Tab.empty

moveToNth :: Int -> TabZipper -> TabZipper
moveToNth n zipper = case moveTo n zipper of
  Just newZipper -> newZipper
  _ -> zipper

swapPrev :: TabZipper -> TabZipper
swapPrev zipper
  | atStart zipper && atEnd zipper = zipper
  | atStart zipper = insert (_focus zipper) . previous $ removeTab zipper
  | atEnd zipper = insertLeft (_focus zipper) $ removeTab zipper
  | otherwise = insertLeft (_focus zipper) . previous $ removeTab zipper

swapNext :: TabZipper -> TabZipper
swapNext zipper
  | atStart zipper && atEnd zipper = zipper
  | atEnd zipper = insertLeft (_focus zipper) . next $ removeTab zipper
  | otherwise = insert (_focus zipper) $ removeTab zipper

notifySize :: FilePath -> Entry.Size -> Pane -> Pane
notifySize path size pane = pane {tabZipper = Tab.notifySize path size <$> tabZipper pane}

waitingEntries :: Pane -> [Entry.Entry]
waitingEntries = concatMap Tab.waitingEntries . toList . tabZipper
