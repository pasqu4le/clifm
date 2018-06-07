module Widgets.Tab where
import Commons
import qualified Widgets.Entry as Entry

import Control.Lens hiding (Empty)
import Data.List (sortOn, isInfixOf, elemIndex)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import System.FilePath (takeFileName, takeDirectory, (</>))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import Brick.Types (Widget, EventM)
import Brick.Widgets.Core (hLimit, hBox, vBox, (<+>), str, strWrap, fill, withBorderStyle, visible)
import Brick.Widgets.List (List, list, renderList, handleListEvent, listMoveTo,
  listSelectedElement, listReplace, listElements, listElementsL, listSelectedL, listNameL, listItemHeightL)
import Brick.Widgets.Border (hBorder, vBorder, borderElem, border)
import Brick.Widgets.Border.Style (unicodeRounded, unicodeBold, bsHorizontal, bsCornerTL, bsCornerTR)
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))
import Data.Foldable (toList)
import qualified Data.Vector as Vect

data Tab = Dir {_name :: String, _path :: FilePath, _entryList :: List Name Entry.Entry, _entryOrder :: EntryOrder} |
  Search {_name :: String, _path :: FilePath, _query :: String, _entryList :: List Name Entry.Entry, _entryOrder :: EntryOrder} |
  Empty
data EntryOrder = EntryOrder {_orderType :: OrderType, _inverted :: Bool}
data OrderType = FileName | FileSize | AccessTime | ModificationTime deriving (Eq, Enum, Bounded)

instance Show Tab where
  show Empty = "\x276f -new tab-"
  show Dir {_name = n} = "\x2636 " ++ n
  show Search {_name = n} = "\x26B2 " ++ n

instance Show EntryOrder where
  show order = show (_orderType order) ++ (if _inverted order then " \x2193 " else " \x2191 ")

instance Show OrderType where
  show FileName = "name"
  show FileSize = "size"
  show AccessTime = "access"
  show ModificationTime = "modified"

-- lenses
name :: Lens' Tab String
name = lens _name (\tab n -> tab {_name = n})

path :: Lens' Tab FilePath
path = lens _path (\tab n -> tab {_path = n})

entryList :: Lens' Tab (List Name Entry.Entry)
entryList = lens _entryList (\tab n -> tab {_entryList = n})

entryOrder :: Lens' Tab EntryOrder
entryOrder = lens _entryOrder (\tab n -> tab {_entryOrder = n})

query :: Lens' Tab String
query = lens _query (\tab n -> tab {_query = n})

orderType :: Lens' Tab OrderType
orderType = entryOrder.entryOrderType

orderInverted :: Lens' Tab Bool
orderInverted = entryOrder.entryOrderInverted

entryOrderType :: Lens' EntryOrder OrderType
entryOrderType = lens _orderType (\tab n -> tab {_orderType = n})

entryOrderInverted :: Lens' EntryOrder Bool
entryOrderInverted = lens _inverted (\tab n -> tab {_inverted = n})

--NOTE: remember that it will fail for Empty tabs
entries :: Traversal' Tab Entry.Entry
entries = entryList.traverse

-- creation
empty :: Tab
empty = Empty

makeDirTab :: PaneName -> FilePath -> IO Tab
makeDirTab pName filePath = do
  isFile <- doesFileExist filePath
  isDir <- doesDirectoryExist filePath
  if isDir && not isFile then do
    let fName = takeFileName filePath
        order = EntryOrder FileName False
    entryLst <- makeDirEntryList pName order filePath
    return $ Dir (if null fName then "-root-" else fName) filePath entryLst order
  else return empty

makeDirEntryList :: PaneName -> EntryOrder -> FilePath -> IO (List Name Entry.Entry)
makeDirEntryList pName order dir = do
  sub <- listDirectory dir
  entries <- mapM (Entry.make . (dir </>)) sub
  upDir <- Entry.makeBackDir $ takeDirectory dir
  return $ list EntryList {pnName = pName} (Vect.fromList . (upDir :) $ sortEntries order entries) 1

makeSearchTab :: PaneName -> FilePath -> String -> IO Tab
makeSearchTab pName filePath searchQuery = do
  isFile <- doesFileExist filePath
  isDir <- doesDirectoryExist filePath
  if isDir && not isFile then do
    let order = EntryOrder FileName False
    entryLst <- makeSearchEntryList pName order filePath searchQuery
    return $ Search searchQuery filePath searchQuery entryLst order
  else return empty

makeSearchEntryList :: PaneName -> EntryOrder -> FilePath -> String -> IO (List Name Entry.Entry)
makeSearchEntryList pName order dir searchQuery = do
  searchResult <- searchRecursive dir searchQuery
  entries <- mapM Entry.make searchResult
  searchDir <- Entry.makeBackDir dir
  return $ list EntryList {pnName = pName} (Vect.fromList . (searchDir :) $ sortEntries order entries) 1

-- rendering
renderLabel :: (Tab, Bool) -> Widget Name
renderLabel (tab, hasFoc) = modifs . hLimit (wdt + 2) $ vBox [top, middle]
  where
    modifs = if hasFoc then withBorderStyle unicodeBold . visible
      else withBorderStyle unicodeRounded
    txt = show tab
    wdt = min 14 $ length txt
    top = hBox [borderElem bsCornerTL, hBorder, borderElem bsCornerTR]
    middle = hBox [vBorder, str $ take wdt txt, fill ' ', vBorder]

renderSeparator :: Tab -> Widget Name
renderSeparator tab = hBox [
    borderElem bsHorizontal,
    renderPath tab,
    hBorder,
    renderEntryOrder tab,
    borderElem bsHorizontal
  ]

renderEntryOrder :: Tab -> Widget Name
renderEntryOrder tab = str $ case tab of
  Empty -> ""
  _ -> " by " ++ views entryOrder show tab

renderPath :: Tab -> Widget Name
renderPath tab = str $ case tab of
  Empty -> " <empty tab> "
  Dir {_path = p} -> " " ++ p ++ " "
  Search {_path = p, _query = q} -> " search for " ++ q ++ " in " ++ takeFileName p

renderContent :: Bool -> Tab -> Widget Name
renderContent _ Empty = vBox (lns ++ [fill ' '])
  where lns = map strWrap $ lines "Command Line Interface File Manager\n \n\
    \clifm allows you to explore directories on multiple tabs.\nIf your terminal\
    \ has mouse support you can click on some elements to interact with them, \
    \but you can perform every action with your keyboard.\n \nInside each tab \
    \you can move to a different entry using the up and down arrow keys \
    \(Home/End to jump to top or bottom) and Enter to move into a selected \
    \directory.\n \nYou can move to a different tab using... the Tab and the \
    \BackTab key or use Ctrl + Left or Right arrow key to swap them.\n \nYou can \
    \see every other possible action as a button in the bottom, or you can use \
    \them as Keys combination.\n \nTo see them all please refer to the README"
renderContent hasFocus tab = views entryList (renderList Entry.render hasFocus) tab

-- event handling and state-changing
handleEvent :: Event -> Tab -> EventM Name Tab
handleEvent _ Empty = return Empty
handleEvent event tab = case event of
  EvKey (KChar 'o') [] -> return $ changeOrder tab
  EvKey (KChar 'i') [] -> return $ invertOrder tab
  _ -> entryList (handleListEvent event) tab

changeOrder :: Tab -> Tab
changeOrder Empty = Empty
changeOrder tab = tab
  & entryOrder .~ newOrder
  & entryList.listElementsL._tail .~ sorted
  & entryList.listSelectedL ?~ maybe 0 (+1) ((`Vect.elemIndex` sorted) =<< selectedEntry tab)
  where
    newOrder = over entryOrderType nextOrderType $ view entryOrder tab
    sorted = Vect.fromList . sortEntries newOrder . toList $ view (entryList.listElementsL._tail) tab

invertOrder :: Tab -> Tab
invertOrder Empty = Empty
invertOrder tab = tab 
  & orderInverted %~ not 
  & entryList.listElementsL._tail %~ Vect.reverse
  & entryList.listSelectedL %~ fmap (\idx -> if idx == 0 then 0 else size - idx)
  where size = views (entryList.listElementsL) Vect.length tab

reload :: PaneName -> Tab -> IO Tab
reload pName tab = case tab of
  Empty -> return Empty
  Dir {_path=p, _entryOrder=o} -> keepSelection tab <$> makeDirEntryList pName o p
  Search {_path=p, _entryOrder=o, _query=q} -> keepSelection tab <$> makeSearchEntryList pName o p q

keepSelection :: Tab -> List Name Entry.Entry -> Tab
keepSelection tab newList = tab 
  & entryList.listElementsL .~ newElems
  & entryList.listSelectedL ?~ fromMaybe 0 ((`Vect.elemIndex` newElems) =<< selectedEntry tab)
  where newElems = listElements newList

moveToRow :: Int -> Tab -> Tab
moveToRow _ Empty = Empty
moveToRow row tab = tab & entryList %~ listMoveTo row

-- utility
isDir :: Tab -> Bool
isDir Dir {} = True
isDir _ = False

isSearch :: Tab -> Bool
isSearch Search {} = True
isSearch _ = False

isEmpty :: Tab -> Bool
isEmpty Empty = True
isEmpty _ = False

selectedEntry :: Tab -> Maybe Entry.Entry
selectedEntry Empty = Nothing
selectedEntry tab = snd <$> listSelectedElement (view entryList tab)

nextOrderType :: OrderType -> OrderType
nextOrderType order
  | order == maxBound = minBound
  | otherwise = succ order

sortEntries :: EntryOrder -> [Entry.Entry] -> [Entry.Entry]
sortEntries order
  | view entryOrderInverted order = reverse . orderOn
  | otherwise = orderOn
  where 
    orderOn = case view entryOrderType order of
      FileName -> sortOn (map toLower . view Entry.name)
      FileSize -> sortOn (view Entry.size)
      AccessTime -> sortOn (fromMaybe zeroTime . preview Entry.accessTime)
      ModificationTime -> sortOn (fromMaybe zeroTime . preview Entry.modifTime)

zeroTime :: UTCTime
zeroTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

searchRecursive :: FilePath -> String -> IO [FilePath]
searchRecursive filePath searchQuery = do
  subNames <- listDirectory filePath
  filePaths <- listRecursive $ map (filePath </>) subNames
  return $ filter (isInfixOf searchQuery . takeFileName) filePaths

listRecursive :: [FilePath] -> IO [FilePath]
listRecursive [] = return []
listRecursive (filePath:filePaths) = do
  isDir <- doesDirectoryExist filePath
  isFile <- doesFileExist filePath
  if isFile then (filePath :) <$> listRecursive filePaths
  else if isDir then do
    subNames <- listDirectory filePath
    let subPaths = map (filePath </>) subNames
    (filePath :) <$> listRecursive (subPaths ++ filePaths)
  else listRecursive filePaths
