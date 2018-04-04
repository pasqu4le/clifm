module Widgets.Tab where
import Types

import Data.List (sortOn)
import Data.Char (toLower)
import Data.Vector (fromList)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Exception (try, SomeException)
import System.FilePath (takeFileName, takeDirectory, (</>))
import System.Directory (Permissions, getPermissions, readable, writable, executable, searchable,
  getAccessTime, getModificationTime, doesDirectoryExist, doesFileExist, getFileSize, listDirectory)
import Brick.Types (Widget, EventM)
import Brick.Widgets.Core (hLimit, vLimit, hBox, vBox, (<+>), str, strWrap, fill, withBorderStyle, withDefAttr, visible)
import Brick.Widgets.List (List, list, renderList, handleListEvent, listMoveTo,
  listSelectedElement, listInsert, listRemove, listReverse, listReplace)
import Brick.Widgets.Border (hBorder, vBorder, borderElem, border)
import Brick.Widgets.Border.Style (unicodeRounded, unicodeBold, bsHorizontal, bsCornerTL, bsCornerTR)
import Graphics.Vty (Event(EvKey), Key(..), Modifier(MCtrl))
import Data.Foldable (toList)

data Tab = DirTab {tabName :: String, tabPath :: FilePath, entryList :: List Name Entry, entryOrder :: EntryOrder} | EmptyTab
data Entry = DirEntry {entryName :: String, entryPath :: FilePath, entryInfo :: EntryInfo} |
  FileEntry {entryName :: String, entryPath :: FilePath, entryInfo :: EntryInfo}
data EntryInfo = EntryInfo {entrySize :: Integer, entryPerms :: Maybe Permissions, entryTimes :: Maybe (UTCTime, UTCTime)} deriving Show
data EntryOrder = EntryOrder {orderType :: OrderType, inverted :: Bool}
data OrderType = FileName | FileSize | AccessTime | ModificationTime deriving (Eq, Enum, Bounded)

instance Eq Tab where
  EmptyTab == EmptyTab = True
  (DirTab {tabPath = p1}) == (DirTab {tabPath = p2}) = p1 == p2
  _ == _ = False

instance Show Tab where
  show EmptyTab = "-new tab-"
  show (DirTab {tabName = name}) = name

instance Show Entry where
  show (DirEntry {entryName = n}) = "+ " ++ n
  show (FileEntry {entryName = n}) = "- " ++ n

instance Show EntryOrder where
  show order = show (orderType order) ++ (if inverted order then " \x2193 " else " \x2191 ")

instance Show OrderType where
  show FileName = "file name"
  show FileSize = "file size"
  show AccessTime = "access time"
  show ModificationTime = "modification time"

-- creation functions
makeEmptyTab :: Tab
makeEmptyTab = EmptyTab

makeDirTab :: FilePath -> IO Tab
makeDirTab path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  if isDir && not isFile then do
    let fName = takeFileName path
        order = EntryOrder FileName False
    entryLst <- makeEntryList order path
    return $ DirTab (if null fName then "-root-" else fName) path entryLst order
  else return makeEmptyTab

makeEntryList :: EntryOrder -> FilePath -> IO (List Name Entry)
makeEntryList order dir = do
  sub <- listDirectory dir
  entries <- mapM (makeEntry . (dir </>)) sub
  let upPath = takeDirectory dir
  upDir <- DirEntry ".." upPath <$> makeEntryInfo upPath
  return $ list EList (fromList . (upDir :) $ sortEntries order entries) 1

makeEntry :: FilePath -> IO Entry
makeEntry path = do
  isFile <- doesFileExist path
  if isFile then FileEntry (takeFileName path) path <$> makeEntryInfo path
  else DirEntry (takeFileName path) path <$> makeEntryInfo path

makeEntryInfo :: FilePath -> IO EntryInfo
makeEntryInfo path = do
  size <- getFileSize path
  perms <- toMaybe <$> (try $ getPermissions path)
  times <- toMaybe <$> (try $ getEntryTimes path)
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
  DirTab {entryOrder = order} -> " by " ++ show order
  _ -> ""

renderPath :: Tab -> Widget Name
renderPath tab = str $ case tab of
  EmptyTab -> " <empty tab> "
  DirTab {tabPath = path} -> " " ++ path ++ " "

renderContent :: Tab -> Widget Name
renderContent (DirTab {entryList = enList}) = renderList renderEntry True enList
renderContent EmptyTab = vBox (lns ++ [fill ' '])
  where lns = map strWrap $ lines "Command Line Interface File Manager\n \n\
    \clifm allows you to explore directories on multiple tabs.\nIf your terminal\
    \ has mouse support you can click on some elements to interact with them, \
    \but you can perform every action with your keyboard.\n \nInside each tab \
    \you can move to a different entry using the up and down arrow keys \
    \(Home/End to jump to top or bottom) and Enter to move into a selected \
    \directory.\n \nYou can move to a different tab using... the Tab and the \
    \BackTab key or use Ctrl + Left or Right arrow key to swap them.\n \nYou can \
    \see every other possible action as a button in the bottom, or you can use \
    \them as Ctrl+Key combination.\n \nTo see them all please refer to the README"

renderEntry :: Bool -> Entry -> Widget Name
renderEntry _ en = let info = entryInfo en in vLimit 1 $ hBox [
    str $ show en,
    fill ' ',
    str (" " ++ show (entrySize info) ++ " B"),
    renderEntryPerms $ entryPerms info,
    renderEntryTime (entryTimes info) False
  ]

renderEntryPerms :: Maybe Permissions -> Widget Name
renderEntryPerms Nothing = str " ----"
renderEntryPerms (Just p) = str [
    ' ',
    (if readable p then 'r' else '-'),
    (if writable p then 'w' else '-'),
    (if executable p then 'x' else '-'),
    (if searchable p then 's' else '-')
  ]

renderEntryTime :: Maybe (UTCTime, UTCTime) -> Bool -> Widget Name
renderEntryTime Nothing _ = str " -----------------"
renderEntryTime (Just tms) sel = str . format $ (if sel then fst else snd) tms
  where format = formatTime defaultTimeLocale " %R %b %e %Y"

tabButtons :: Tab -> [(Widget Name, Char)]
tabButtons DirTab {} = [
    (str "cut", 'x'),
    (str "copy", 'c'),
    (str "paste", 'v'),
    (withDefAttr keybindAttr (str "r") <+> str "ename", 'r'),
    (withDefAttr keybindAttr (str "d") <+> str "elete", 'd'),
    (str "m" <+> withDefAttr keybindAttr (str "a") <+> str "ke dir", 'm'),
    (withDefAttr keybindAttr (str "t") <+> str "ouch file", 't'),
    (withDefAttr keybindAttr (str "s") <+> str "how info", 's'),
    (str "re" <+> withDefAttr keybindAttr (str "l") <+> str "oad", 'l'),
    (withDefAttr keybindAttr (str "o") <+> str "pen in new tab", 'o')
  ]
tabButtons _ = []

-- event handling and state-changing functions
handleTabEvent :: Event -> Tab -> EventM Name Tab
handleTabEvent _ EmptyTab = return EmptyTab
handleTabEvent event tab = case event of
  EvKey (KChar 'b') [MCtrl] -> return $ changeOrder tab
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
    (upDir:entries) = toList eLst
    sorted = upDir : sortEntries newOrder entries
    newEntryList = listReplace (fromList sorted) (Just 0) eLst

invertOrder :: Tab -> Tab
invertOrder EmptyTab = EmptyTab
invertOrder tab = tab {entryOrder = newOrder, entryList = newEntryList}
  where
    order = entryOrder tab
    newOrder = order {inverted = not $ inverted order}
    eLst = entryList tab
    upDir = head $ toList eLst
    newEntryList = listInsert 0 upDir . listReverse $ listRemove 0 eLst

reload :: Tab -> IO Tab
reload tab = case tab of
  EmptyTab -> return EmptyTab
  tab -> do
    reloadedList <- makeEntryList (entryOrder tab) (tabPath tab)
    return tab {entryList = reloadedList}

moveToRow :: Int -> Tab -> Tab
moveToRow _ EmptyTab = EmptyTab
moveToRow row dirTab = dirTab {entryList = (listMoveTo row $ entryList dirTab)}

-- utility functions
maybeTabPath :: Tab -> Maybe FilePath
maybeTabPath EmptyTab = Nothing
maybeTabPath (DirTab {tabPath = path}) = Just path

selectedEntry :: Tab -> Maybe Entry
selectedEntry EmptyTab = Nothing
selectedEntry (DirTab {entryList = enList}) = case listSelectedElement enList of
  Just (_, entry) -> Just entry
  _ -> Nothing

toMaybe :: Either SomeException b -> Maybe b
toMaybe = either (\_ -> Nothing) (Just)

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
