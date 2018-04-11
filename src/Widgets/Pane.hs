module Widgets.Pane where
import Types
import Widgets.Tab

import Control.Monad.IO.Class (liftIO)
import Brick.Widgets.Core (hBox, vBox, vLimit, viewport, clickable)
import Brick.Types (Widget, BrickEvent(..), EventM, ViewportType(..))
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))
import Data.Foldable (toList)
import Data.List.PointedList (PointedList, _focus, replace, delete, singleton, insert, insertLeft, moveTo, withFocus, atStart, atEnd)
import Data.List.PointedList.Circular (next, previous)

data Pane = Pane {paneName :: PaneName, tabZipper :: TabZipper}
type TabZipper = PointedList Tab

instance Eq Pane where
  Pane {paneName = p1} == Pane {paneName = p2} = p1 == p2

-- creation functions
makePane :: PaneName -> FilePath -> IO Pane
makePane pName path = Pane pName . singleton <$> makeDirTab pName path

makeEmptyPane :: PaneName -> Pane
makeEmptyPane pName = Pane pName . singleton $ makeEmptyTab

-- rendering functions
renderPane :: (Pane, Bool) -> Widget Name
renderPane (pane, hasFocus) = vBox [labels, topSep, content]
  where
    pName = paneName pane
    labels = vLimit 2 . viewport LabelsRow {pnName = pName} Horizontal . renderLabels pName $ tabZipper pane
    topSep = renderPathSeparator $ currentTab pane
    content = clickable EntryList {pnName = pName} . renderContent hasFocus $ currentTab pane

renderLabels :: PaneName -> TabZipper -> Widget Name
renderLabels pName zipper = hBox . map (clickableLabel pName) $ zip labels [0..]
  where labels = map renderLabel . toList $ withFocus zipper

clickableLabel :: PaneName -> (Widget Name, Int) -> Widget Name
clickableLabel pName (l, n) = clickable Label {pnName = pName, labelNum = n} l

-- event handling functions
handlePaneEvent :: Event -> Pane -> EventM Name Pane
handlePaneEvent event = case event of
  EvKey (KChar 'k') [] -> updateTabZipper removeTab
  EvKey (KChar 'e') [] -> updateTabZipper (insert makeEmptyTab)
  EvKey (KChar 'r') [] -> reloadCurrentTab
  EvKey (KChar '\t') [] -> updateTabZipper next
  EvKey KBackTab [] -> updateTabZipper previous
  EvKey KLeft [MCtrl] -> updateTabZipper swapWithPrevious
  EvKey KRight [MCtrl] -> updateTabZipper swapWithNext
  _ -> updateCurrentTab event

-- state-changing functions
updateTabZipper :: (TabZipper -> TabZipper) -> Pane -> EventM Name Pane
updateTabZipper func pane = return $ pane {tabZipper = func $ tabZipper pane}

reloadCurrentTab :: Pane -> EventM Name Pane
reloadCurrentTab pane = do
  reloaded <- liftIO . reload (paneName pane) $ currentTab pane
  updateTabZipper (replace reloaded) pane

updateCurrentTab :: Event -> Pane -> EventM Name Pane
updateCurrentTab event pane = do
  updated <- handleTabEvent event $ currentTab pane
  updateTabZipper (replace updated) pane

openSelectedDir :: Bool -> Pane -> EventM Name Pane
openSelectedDir inNew pane = case selectedEntry $ currentTab pane of
  Just DirEntry {entryPath = path} -> do
    loaded <- liftIO $ makeDirTab (paneName pane) path
    let modify = if inNew then insertFixed else replace
    updateTabZipper (modify loaded) pane
  _ -> return pane

-- tab and tabZipper utility functions
moveTabToRow :: Int -> Pane -> EventM Name Pane
moveTabToRow row pane = updateTabZipper (replace (moveToRow row $ currentTab pane)) pane

currentTab :: Pane -> Tab
currentTab = _focus . tabZipper

removeTab :: TabZipper -> TabZipper
removeTab zipper = case delete zipper of
  Just newZipper -> newZipper
  _ -> singleton makeEmptyTab

moveToNth :: Int -> TabZipper -> TabZipper
moveToNth n zipper = case moveTo n zipper of
  Just newZipper -> newZipper
  _ -> zipper

insertFixed :: Tab -> TabZipper -> TabZipper
insertFixed tab = previous . insert tab

swapWithPrevious :: TabZipper -> TabZipper
swapWithPrevious zipper
  | atStart zipper && atEnd zipper = zipper
  | atStart zipper = insert (_focus zipper) . previous $ removeTab zipper
  | atEnd zipper = insertLeft (_focus zipper) $ removeTab zipper
  | otherwise = insertLeft (_focus zipper) . previous $ removeTab zipper

swapWithNext :: TabZipper -> TabZipper
swapWithNext zipper
  | atStart zipper && atEnd zipper = zipper
  | atEnd zipper = insertLeft (_focus zipper) . next $ removeTab zipper
  | otherwise = insert (_focus zipper) $ removeTab zipper
