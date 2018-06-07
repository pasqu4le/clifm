module Widgets.Pane where
import Commons
import qualified Widgets.Tab as Tab
import qualified Widgets.Entry as Entry

import Control.Lens
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Brick.Widgets.Core (hBox, vBox, vLimit, viewport, clickable)
import Brick.Types (Widget, BrickEvent(..), EventM, ViewportType(..))
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))
import Data.Foldable (toList)
import Data.List.PointedList (PointedList, _focus, focus, replace, delete, singleton, insert, insertLeft, moveTo, withFocus, atStart, atEnd)
import Data.List.PointedList.Circular (next, previous)

data Pane = Pane {_name :: PaneName, _tabZipper :: TabZipper}
type TabZipper = PointedList Tab.Tab

instance Eq Pane where
  Pane {_name = p1} == Pane {_name = p2} = p1 == p2

-- lenses 
name :: Lens' Pane PaneName
name = lens _name (\pane x -> pane {_name = x})

tabZipper :: Lens' Pane TabZipper
tabZipper = lens _tabZipper (\pane x -> pane {_tabZipper = x})

currentTab :: Lens' Pane Tab.Tab
currentTab = tabZipper.focus

--NOTE: needs to filter empty tabs, or it will fail
entries :: Traversal' Pane Entry.Entry
entries = tabZipper.traverse.filtered (not . Tab.isEmpty).Tab.entries

-- creation
empty :: PaneName -> Pane
empty pName = Pane pName . singleton $ Tab.empty

make :: PaneName -> FilePath -> IO Pane
make pName path = Pane pName . singleton <$> Tab.makeDirTab pName path

-- rendering
render :: (Pane, Bool) -> Widget Name
render (pane, hasFocus) = let pName = view name pane in vBox [
    vLimit 2 . viewport LabelsRow {pnName = pName} Horizontal $ views tabZipper (renderLabels pName) pane,
    views currentTab Tab.renderSeparator pane,
    clickable EntryList {pnName = pName} $ views currentTab (Tab.renderContent hasFocus) pane
  ]

renderLabels :: PaneName -> TabZipper -> Widget Name
renderLabels pName zipper = hBox . map (clickableLabel pName) $ zip labels [0..]
  where labels = map Tab.renderLabel . toList $ withFocus zipper

clickableLabel :: PaneName -> (Widget Name, Int) -> Widget Name
clickableLabel pName (l, n) = clickable Label {pnName = pName, labelNum = n} l

-- event handling
handleEvent :: Event -> Pane -> EventM Name Pane
handleEvent event = case event of
  EvKey (KChar 'k') [] -> return . over tabZipper removeTab
  EvKey (KChar 'e') [] -> return . over tabZipper (insert Tab.empty)
  EvKey (KChar 'r') [] -> reloadCurrentTab
  EvKey (KChar '\t') [] -> return . over tabZipper next
  EvKey KBackTab [] -> return . over tabZipper previous
  EvKey KLeft [MCtrl] -> return . over tabZipper swapPrev
  EvKey KRight [MCtrl] -> return . over tabZipper swapNext
  _ -> currentTab (Tab.handleEvent event)

-- state-changing
reloadCurrentTab :: Pane -> EventM Name Pane
reloadCurrentTab pane = currentTab (liftIO . Tab.reload (view name pane)) pane

openDirEntry :: Bool -> Pane -> EventM Name Pane
openDirEntry inNew pane = case selectedEntry pane of
  Just entry -> if not $ Entry.isDir entry then return pane else do
    loaded <- liftIO $ Tab.makeDirTab (view name pane) $ view Entry.path entry
    return $ over tabZipper (if inNew then previous . insert loaded else replace loaded) pane
  _ -> return pane

moveToNth :: Int -> Pane -> Pane
moveToNth n = over tabZipper (\z -> fromMaybe z $ moveTo n z)

-- utility
selectedEntry :: Pane -> Maybe Entry.Entry
selectedEntry = Tab.selectedEntry . view currentTab

removeTab :: TabZipper -> TabZipper
removeTab zipper = case delete zipper of
  Just newZipper -> newZipper
  _ -> singleton Tab.empty

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
