module Widgets.Tab where
import Commons
import qualified Widgets.Entry as Entry

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
  listSelectedElement, listReplace, listElements)
import Brick.Widgets.Border (hBorder, vBorder, borderElem, border)
import Brick.Widgets.Border.Style (unicodeRounded, unicodeBold, bsHorizontal, bsCornerTL, bsCornerTR)
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))
import Data.Foldable (toList)
import qualified Data.Vector as Vect

data Tab = Dir {name :: String, path :: FilePath, entryList :: List Name Entry.Entry, entryOrder :: EntryOrder} |
  Search {name :: String, path :: FilePath, query :: String, entryList :: List Name Entry.Entry, entryOrder :: EntryOrder} |
  Empty
data EntryOrder = EntryOrder {orderType :: OrderType, inverted :: Bool}
data OrderType = FileName | FileSize | AccessTime | ModificationTime deriving (Eq, Enum, Bounded)

instance Show Tab where
  show Empty = "\x276f -new tab-"
  show Dir {name = n} = "\x2636 " ++ n
  show Search {name = n} = "\x26B2 " ++ n

instance Show EntryOrder where
  show order = show (orderType order) ++ (if inverted order then " \x2193 " else " \x2191 ")

instance Show OrderType where
  show FileName = "name"
  show FileSize = "size"
  show AccessTime = "access"
  show ModificationTime = "modified"

-- creation functions
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

-- rendering functions
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
renderSeparator t = hBox [
    borderElem bsHorizontal,
    renderPath t,
    hBorder,
    renderEntryOrder t,
    borderElem bsHorizontal
  ]

renderEntryOrder :: Tab -> Widget Name
renderEntryOrder tab = str $ case tab of
  Empty -> ""
  _ -> " by " ++ show (entryOrder tab)

renderPath :: Tab -> Widget Name
renderPath tab = str $ case tab of
  Empty -> " <empty tab> "
  Dir {path = p} -> " " ++ p ++ " "
  Search {path = p, query = q} -> " search for " ++ q ++ " in " ++ takeFileName p

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
renderContent hasFocus tab = renderList Entry.render hasFocus $ entryList tab

-- event handling and state-changing functions
handleEvent :: Event -> Tab -> EventM Name Tab
handleEvent _ Empty = return Empty
handleEvent event tab = case event of
  EvKey (KChar 'o') [] -> return $ changeOrder tab
  EvKey (KChar 'i') [] -> return $ invertOrder tab
  _ -> do
    newList <- handleListEvent event $ entryList tab
    return $ tab {entryList = newList}

changeOrder :: Tab -> Tab
changeOrder Empty = Empty
changeOrder tab = tab {entryOrder = newOrder,  entryList = newEntryList}
  where
    order = entryOrder tab
    newOrder = order {orderType = nextOrderType $ orderType order}
    eLst = entryList tab
    (fstDir:entries) = toList eLst
    sorted = fstDir : sortEntries newOrder entries
    selected = fromMaybe fstDir $ selectedEntry tab
    newIndex = Just . fromMaybe 0 $ elemIndex selected sorted
    newEntryList = listReplace (Vect.fromList sorted) newIndex eLst

invertOrder :: Tab -> Tab
invertOrder Empty = Empty
invertOrder tab = tab {entryOrder = newOrder, entryList = newEntryList}
  where
    order = entryOrder tab
    newOrder = order {inverted = not $ inverted order}
    eLst = entryList tab
    entries = listElements eLst
    index = selectedIndex eLst
    newIndex = Just $ if index == 0 then 0 else Vect.length entries - index
    reversed = Vect.cons (Vect.head entries) . Vect.reverse $ Vect.tail entries
    newEntryList = listReplace reversed newIndex eLst

reload :: PaneName -> Tab -> IO Tab
reload pName tab = case tab of
  Empty -> return Empty
  Dir {path=p, entryOrder=o} -> keepSelection tab <$> makeDirEntryList pName o p
  Search {path=p, entryOrder=o, query=q} -> keepSelection tab <$> makeSearchEntryList pName o p q

keepSelection :: Tab -> List Name Entry.Entry -> Tab
keepSelection tab newList = tab {entryList = listMoveTo index newList}
  where
    fstDir = Vect.head . listElements $ entryList tab
    selected = fromMaybe fstDir $ selectedEntry tab
    index = fromMaybe 0 . Vect.elemIndex selected $ listElements newList

moveToRow :: Int -> Tab -> Tab
moveToRow _ Empty = Empty
moveToRow row tab = tab {entryList = listMoveTo row $ entryList tab}

notifySize :: FilePath -> Entry.Size -> Tab -> Tab
notifySize _ _ Empty = Empty
notifySize path size tab = tab {entryList = Entry.notifySize path size <$> entryList tab}

waitingEntries :: Tab -> [Entry.Entry]
waitingEntries Empty = []
waitingEntries tab = filter (\x -> Entry.Waiting == Entry.size (Entry.info x)) . toList $ entryList tab

-- utility functions
selectedEntry :: Tab -> Maybe Entry.Entry
selectedEntry Empty = Nothing
selectedEntry tab = case listSelectedElement $ entryList tab of
  Just (_, entry) -> Just entry
  _ -> Nothing

selectedIndex :: List Name Entry.Entry -> Int
selectedIndex entries = case listSelectedElement entries of
  Just (n, _) -> n
  _ -> 0

nextOrderType :: OrderType -> OrderType
nextOrderType order
  | order == maxBound = minBound
  | otherwise = succ order

sortEntries :: EntryOrder -> [Entry.Entry] -> [Entry.Entry]
sortEntries order = (if inverted order then reverse else id) . case orderType order of
  FileName -> sortOn (map toLower . Entry.name)
  FileSize -> sortOn (Entry.size . Entry.info)
  AccessTime -> sortOn (fst . fromMaybe (zeroTime, zeroTime) . Entry.times . Entry.info)
  ModificationTime -> sortOn (snd . fromMaybe (zeroTime, zeroTime) . Entry.times . Entry.info)

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
