module Widgets.Entry where
import Commons

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Exception (try, SomeException)
import Conduit
import System.FilePath (takeFileName)
import System.Directory (Permissions, getPermissions, readable, writable, executable, searchable,
  getAccessTime, getModificationTime, doesFileExist, getFileSize)
import Brick.Types (Widget)
import Brick.Widgets.Core (vLimit, hBox, str, fill)
import Data.ByteUnits (ByteValue(..), ByteUnit(Bytes), getShortHand, getAppropriateUnits)

data Entry = Dir {_name :: String, _path :: FilePath, _info :: Info} |
  File {_name :: String, _path :: FilePath, _info :: Info} deriving (Ord)
data Info = Info {_size :: Size, _perms :: Maybe Permissions, _times :: Maybe (UTCTime, UTCTime)} deriving (Show, Eq, Ord)
data Size = Waiting | Calculating | Known Integer | Unknown | Avoided deriving (Show, Eq)

instance Show Entry where
  show Dir {_name = n} = "+ " ++ n
  show File {_name = n} = "- " ++ n

instance Eq Entry where
  Dir {_path = p1} == Dir {_path = p2} = p1 == p2
  File {_path = p1} == File {_path = p2} = p1 == p2
  _ == _ = False

instance Ord Size where
  compare (Known a) (Known b) = compare a b
  compare (Known _) _ = GT
  compare _ (Known _) = LT
  compare _ _ = EQ

-- lenses
name :: Lens' Entry String
name = lens _name (\entry n -> entry {_name = n})

path :: Lens' Entry FilePath
path = lens _path (\entry p -> entry {_path = p})

info :: Lens' Entry Info
info = lens _info (\entry i -> entry {_info = i})

size :: Lens' Entry Size
size = info.infoSize

perms :: Lens' Entry (Maybe Permissions)
perms = info.infoPerms

times :: Lens' Entry (Maybe (UTCTime, UTCTime))
times = info.infoTimes

accessTime :: Traversal' Entry UTCTime
accessTime = times._Just._1

modifTime :: Traversal' Entry UTCTime
modifTime = times._Just._2

infoSize :: Lens' Info Size
infoSize = lens _size (\entry s -> entry {_size = s})

infoPerms :: Lens' Info (Maybe Permissions)
infoPerms = lens _perms (\entry p -> entry {_perms = p})

infoTimes :: Lens' Info (Maybe (UTCTime, UTCTime))
infoTimes = lens _times (\entry t -> entry {_times = t})

-- creation
make :: FilePath -> IO Entry
make filePath = do
  isFile <- doesFileExist filePath
  if isFile then File (takeFileName filePath) filePath <$> makeInfo filePath True
  else Dir (takeFileName filePath) filePath <$> makeInfo filePath False

makeInfo :: FilePath -> Bool -> IO Info
makeInfo filePath isFile = do
  enSize <- getEntrySize filePath isFile
  enPerms <- toMaybe <$> try (getPermissions filePath)
  enTimes <- toMaybe <$> try (getTimes filePath)
  return $ Info enSize enPerms enTimes

getTimes :: FilePath -> IO (UTCTime, UTCTime)
getTimes filePath = do
  accTime <- getAccessTime filePath
  modTime <- getModificationTime filePath
  return (accTime, modTime)

makeBackDir :: FilePath -> IO Entry
makeBackDir filePath = do
  enPerms <- toMaybe <$> try (getPermissions filePath)
  enTimes <- toMaybe <$> try (getTimes filePath)
  return $ Dir ".." filePath (Info Avoided enPerms enTimes)

-- rendering
render :: Bool -> Entry -> Widget Name
render _ entry = vLimit 1 $ hBox [
    str $ show entry,
    fill ' ',
    views size (str . shortSize) entry,
    views perms renderPerms entry,
    renderModTime $ preview modifTime entry
  ]

renderPerms :: Maybe Permissions -> Widget Name
renderPerms Nothing = str " ----"
renderPerms (Just p) = str [
    ' ',
    if readable p then 'r' else '-',
    if writable p then 'w' else '-',
    if executable p then 'x' else '-',
    if searchable p then 's' else '-'
  ]

renderModTime :: Maybe UTCTime -> Widget Name
renderModTime modTime = str $ case modTime of
  Just mtm -> formatTime defaultTimeLocale " %R %b %e %Y" mtm
  _ -> " -----------------"

-- utility
isDir :: Entry -> Bool
isDir Dir {} = True
isDir _ = False

isFile :: Entry -> Bool
isFile File {} = True
isFile _ = False

isWaiting :: Entry -> Bool
isWaiting entry = entry ^. size == Waiting

toMaybe :: Either SomeException b -> Maybe b
toMaybe = either (const Nothing) Just

isExecutable :: Entry -> Bool
isExecutable = hasPermission executable

isReadable :: Entry -> Bool
isReadable = hasPermission readable

hasPermission :: (Permissions -> Bool) -> Entry -> Bool
hasPermission p = maybe False p . view perms

shortSize :: Size -> String
shortSize sz = case sz of
  Known enSize -> getShortHand . getAppropriateUnits $ ByteValue (fromInteger enSize) Bytes
  Unknown -> "???"
  Calculating -> "..."
  Waiting -> "..."
  _ -> ""

-- directory size
getEntrySize :: FilePath -> Bool -> IO Size
getEntrySize filePath isFile
  | isFile = toSizeResult <$> try (getFileSize filePath)
  | otherwise = return Waiting

toSizeResult :: Either SomeException Integer -> Size
toSizeResult = either (const Unknown) Known

getDirSize :: FilePath -> IO Integer
getDirSize filePath = runConduitRes
  $ sourceDirectoryDeep False filePath
  .| mapMC (liftIO . getFileSize)
  .| sumC
