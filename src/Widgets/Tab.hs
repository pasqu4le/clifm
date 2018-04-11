module Widgets.Tab where
import Types

import Data.List (sortOn, isInfixOf, elemIndex)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Exception (try, SomeException)
import System.FilePath (takeFileName, takeDirectory, (</>))
import System.Directory (Permissions, getPermissions, readable, writable, executable, searchable,
  getAccessTime, getModificationTime, doesDirectoryExist, doesFileExist, getFileSize, listDirectory)
import Brick.Types (Widget, EventM)
import Brick.Widgets.Core (hLimit, vLimit, hBox, vBox, (<+>), str, strWrap, fill, withBorderStyle, visible)
import Brick.Widgets.List (List, list, renderList, handleListEvent, listMoveTo,
  listSelectedElement, listInsert, listRemove, listReverse, listReplace, listElements)
import Brick.Widgets.Border (hBorder, vBorder, borderElem, border)
import Brick.Widgets.Border.Style (unicodeRounded, unicodeBold, bsHorizontal, bsCornerTL, bsCornerTR)
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))
import Data.Foldable (toList)
import qualified Data.Vector as Vect
import Data.ByteUnits (ByteValue(..), ByteUnit(Bytes), getShortHand, getAppropriateUnits)

data Tab = DirTab {tabName :: String, tabPath :: FilePath, entryList :: List Name Entry, entryOrder :: EntryOrder} |
  SearchTab {tabName :: String, tabPath :: FilePath, tabQuery :: String, entryList :: List Name Entry, entryOrder :: EntryOrder} |
  EmptyTab
data Entry = DirEntry {entryName :: String, entryPath :: FilePath, entryInfo :: EntryInfo} |
  FileEntry {entryName :: String, entryPath :: FilePath, entryInfo :: EntryInfo}
data EntryInfo = EntryInfo {entrySize :: Integer, entryPerms :: Maybe Permissions, entryTimes :: Maybe (UTCTime, UTCTime)} deriving Show
data EntryOrder = EntryOrder {orderType :: OrderType, inverted :: Bool}
data OrderType = FileName | FileSize | AccessTime | ModificationTime deriving (Eq, Enum, Bounded)

instance Show Tab where
  show EmptyTab = "\x276f -new tab-"
  show DirTab {tabName = name} = "\x2636 " ++ name
  show SearchTab {tabName = name} = "\x26B2 " ++ name

instance Show Entry where
  show DirEntry {entryName = n} = "+ " ++ n
  show FileEntry {entryName = n} = "- " ++ n

instance Eq Entry where
  DirEntry {entryPath = p1} == DirEntry {entryPath = p2} = p1 == p2
  FileEntry {entryPath = p1} == FileEntry {entryPath = p2} = p1 == p2
  _ == _ = False

instance Show EntryOrder where
  show order = show (orderType order) ++ (if inverted order then " \x2193 " else " \x2191 ")

instance Show OrderType where
  show FileName = "name"
  show FileSize = "size"
  show AccessTime = "access"
  show ModificationTime = "modified"

-- creation functions
makeEmptyTab :: Tab
makeEmptyTab = EmptyTab

makeDirTab :: PaneName -> FilePath -> IO Tab
makeDirTab pName path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  if isDir && not isFile then do
    let fName = takeFileName path
        order = EntryOrder FileName False
    entryLst <- makeDirEntryList pName order path
    return $ DirTab (if null fName then "-root-" else fName) path entryLst order
  else return makeEmptyTab

makeDirEntryList :: PaneName -> EntryOrder -> FilePath -> IO (List Name Entry)
makeDirEntryList pName order dir = do
  sub <- listDirectory dir
  entries <- mapM (makeEntry . (dir </>)) sub
  let upPath = takeDirectory dir
  upDir <- DirEntry ".." upPath <$> makeEntryInfo upPath
  return $ list EntryList {pnName = pName} (Vect.fromList . (upDir :) $ sortEntries order entries) 1

makeSearchTab :: PaneName -> FilePath -> String -> IO Tab
makeSearchTab pName path query = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  if isDir && not isFile then do
    let order = EntryOrder FileName False
    entryLst <- makeSearchEntryList pName order path query
    return $ SearchTab query path query entryLst order
  else return makeEmptyTab

makeSearchEntryList :: PaneName -> EntryOrder -> FilePath -> String -> IO (List Name Entry)
makeSearchEntryList pName order dir query = do
  searchResult <- searchRecursive dir query
  entries <- mapM makeEntry searchResult
  searchDir <- DirEntry "." dir <$> makeEntryInfo dir
  return $ list EntryList {pnName = pName} (Vect.fromList . (searchDir :) $ sortEntries order entries) 1

makeEntry :: FilePath -> IO Entry
makeEntry path = do
  isFile <- doesFileExist path
  if isFile then FileEntry (takeFileName path) path <$> makeEntryInfo path
  else DirEntry (takeFileName path) path <$> makeEntryInfo path

makeEntryInfo :: FilePath -> IO EntryInfo
makeEntryInfo path = do
  size <- getFileSize path
  perms <- toMaybe <$> try (getPermissions path)
  times <- toMaybe <$> try (getEntryTimes path)
  return $ EntryInfo size perms times

getEntryTimes :: FilePath -> IO (UTCTime, UTCTime)
getEntryTimes path = do
  accessTime <- getAccessTime path
  modifTime <- getModificationTime path
  return (accessTime, modifTime)

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

renderPathSeparator :: Tab -> Widget Name
renderPathSeparator t = hBox [
  borderElem bsHorizontal,
  renderPath t,
  hBorder,
  renderEntryOrder t,
  borderElem bsHorizontal]

renderEntryOrder :: Tab -> Widget Name
renderEntryOrder tab = str $ case tab of
  EmptyTab -> ""
  _ -> " by " ++ show (entryOrder tab)

renderPath :: Tab -> Widget Name
renderPath tab = str $ case tab of
  EmptyTab -> " <empty tab> "
  DirTab {tabPath = path} -> " " ++ path ++ " "
  SearchTab {tabPath = p, tabQuery = q} -> " search for " ++ q ++ " in " ++ takeFileName p

renderContent :: Bool -> Tab -> Widget Name
renderContent _ EmptyTab = vBox (lns ++ [fill ' '])
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
renderContent hasFocus tab = renderList renderEntry hasFocus $ entryList tab

renderEntry :: Bool -> Entry -> Widget Name
renderEntry _ en = let info = entryInfo en in vLimit 1 $ hBox [
    str $ show en,
    fill ' ',
    str $ shortEntrySize info,
    renderEntryPerms $ entryPerms info,
    renderEntryTime (entryTimes info) False
  ]

renderEntryPerms :: Maybe Permissions -> Widget Name
renderEntryPerms Nothing = str " ----"
renderEntryPerms (Just p) = str [
    ' ',
    if readable p then 'r' else '-',
    if writable p then 'w' else '-',
    if executable p then 'x' else '-',
    if searchable p then 's' else '-'
  ]

renderEntryTime :: Maybe (UTCTime, UTCTime) -> Bool -> Widget Name
renderEntryTime Nothing _ = str " -----------------"
renderEntryTime (Just tms) sel = str . format $ (if sel then fst else snd) tms
  where format = formatTime defaultTimeLocale " %R %b %e %Y"

-- event handling and state-changing functions
handleTabEvent :: Event -> Tab -> EventM Name Tab
handleTabEvent _ EmptyTab = return EmptyTab
handleTabEvent event tab = case event of
  EvKey (KChar 'o') [] -> return $ changeOrder tab
  EvKey (KChar 'i') [] -> return $ invertOrder tab
  _ -> do
    newList <- handleListEvent event $ entryList tab
    return $ tab {entryList = newList}

changeOrder :: Tab -> Tab
changeOrder EmptyTab = EmptyTab
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
invertOrder EmptyTab = EmptyTab
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
  EmptyTab -> return EmptyTab
  DirTab {tabPath=p, entryOrder=o} -> keepSelection tab <$> makeDirEntryList pName o p
  SearchTab {tabPath=p, entryOrder=o, tabQuery=q} -> keepSelection tab <$> makeSearchEntryList pName o p q

keepSelection :: Tab -> List Name Entry -> Tab
keepSelection tab newList = tab {entryList = listMoveTo index newList}
  where
    fstDir = Vect.head . listElements $ entryList tab
    selected = fromMaybe fstDir $ selectedEntry tab
    index = fromMaybe 0 . Vect.elemIndex selected $ listElements newList

moveToRow :: Int -> Tab -> Tab
moveToRow _ EmptyTab = EmptyTab
moveToRow row tab = tab {entryList = listMoveTo row $ entryList tab}

-- utility functions
selectedEntry :: Tab -> Maybe Entry
selectedEntry EmptyTab = Nothing
selectedEntry tab = case listSelectedElement $ entryList tab of
  Just (_, entry) -> Just entry
  _ -> Nothing

selectedIndex :: List Name Entry -> Int
selectedIndex entries = case listSelectedElement entries of
  Just (n, _) -> n
  _ -> 0

toMaybe :: Either SomeException b -> Maybe b
toMaybe = either (const Nothing) Just

isExecutable :: Entry -> Bool
isExecutable = hasPermission executable

isReadable :: Entry -> Bool
isReadable = hasPermission readable

hasPermission :: (Permissions -> Bool) -> Entry -> Bool
hasPermission prop en = case entryPerms $ entryInfo en of
  Just perms -> prop perms
  _ -> False

nextOrderType :: OrderType -> OrderType
nextOrderType order
  | order == (maxBound :: OrderType) = minBound
  | otherwise = succ order

sortEntries :: EntryOrder -> [Entry] -> [Entry]
sortEntries order = (if inverted order then reverse else id) . case orderType order of
  FileName -> sortOn (map toLower . entryName)
  FileSize -> sortOn (entrySize . entryInfo)
  AccessTime -> sortOn (fst . fromMaybe (zeroTime, zeroTime) . entryTimes . entryInfo)
  ModificationTime -> sortOn (snd . fromMaybe (zeroTime, zeroTime) . entryTimes . entryInfo)

zeroTime :: UTCTime
zeroTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

searchRecursive :: FilePath -> String -> IO [FilePath]
searchRecursive path query = do
  subNames <- listDirectory path
  paths <- listRecursive $ map (path </>) subNames
  return $ filter (isInfixOf query . takeFileName) paths

listRecursive :: [FilePath] -> IO [FilePath]
listRecursive [] = return []
listRecursive (path:paths) = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if isFile then (path :) <$> listRecursive paths
  else if isDir then do
    subNames <- listDirectory path
    let subPaths = map (path </>) subNames
    (path :) <$> listRecursive (subPaths ++ paths)
  else listRecursive paths

shortEntrySize :: EntryInfo -> String
shortEntrySize info = getShortHand . getAppropriateUnits $ ByteValue size Bytes
  where size = fromInteger $ entrySize info
