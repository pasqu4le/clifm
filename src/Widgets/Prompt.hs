module Widgets.Prompt where
import Types
import Widgets.Tab
import Widgets.Clipboard

import Data.Monoid ((<>))
import Control.Monad(when,forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, throw, displayException, SomeException)
import Control.Applicative ((*>), (<$>))
import Brick.Widgets.Core ((<+>), str, strWrap, vBox, hLimit, padLeftRight, padTopBottom, withDefAttr)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Types (Widget, EventM)
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.Edit (Editor, editor, renderEditor, getEditContents, handleEditorEvent)
import Graphics.Vty (Event(EvKey), Key(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.FilePath (isValid, takeDirectory, (</>), takeFileName)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory, renameFile,
  copyFileWithMetadata, renameFile, removeFile, removeDirectoryRecursive, getDirectoryContents,
  readable, writable, executable, searchable)

data Prompt = Prompt {originTab :: Tab, action :: PromptAction} deriving Show
type PathEditor = Editor FilePath Name
data PromptAction = Copy Entry FilePath | Cut Entry FilePath | Rename PathEditor Entry |
  Delete Entry | Mkdir PathEditor FilePath | Touch PathEditor FilePath |
  GoTo PathEditor | Search PathEditor FilePath | DisplayInfo EntryInfo | DisplayError String

instance Show PromptAction where
  show (Copy _ _) = " Copy "
  show (Cut _ _) = " Cut "
  show (Rename _ _) = " Rename "
  show (Delete _) = " Delete "
  show (Mkdir _ _) = " Make Directory "
  show (Touch _ _) = " Touch File "
  show (GoTo _) = " Go To "
  show (DisplayInfo _) = " Entry Info "
  show (Search _ _) = " Search "
  show _ = " Error "

-- creation functions
emptyPathEditor :: PathEditor
emptyPathEditor = makePathEditor []

makePathEditor :: FilePath -> PathEditor
makePathEditor = editor PEdit (Just 1)

makePastePrompt :: Clipboard -> Tab -> Prompt
makePastePrompt c tab = Prompt tab $ case (c, tab) of
  (EmptyBoard, _) -> DisplayError "The clipboard is empty"
  (_, EmptyTab) -> DisplayError "You cannot paste into an empty tab"
  (_, SearchTab {}) -> DisplayError "You cannot paste into a search tab"
  (CopyBoard {fromEntry = entry}, DirTab{tabPath = path}) -> Copy entry path
  (CutBoard {fromEntry = entry}, DirTab{tabPath = path}) -> Cut entry path

makeGoToPrompt :: Tab -> Prompt
makeGoToPrompt tab = Prompt tab $ GoTo emptyPathEditor

makeRenamePrompt :: Tab -> Prompt
makeRenamePrompt = withSelectedEntry (\en -> Rename (editorFromEntry en) en)
  where editorFromEntry = makePathEditor . takeFileName . entryPath

makeDeletePrompt :: Tab -> Prompt
makeDeletePrompt = withSelectedEntry Delete

makeMkdirPrompt :: Tab -> Prompt
makeMkdirPrompt = withDirTabPath (Mkdir emptyPathEditor)

makeTouchPrompt :: Tab -> Prompt
makeTouchPrompt = withDirTabPath (Touch emptyPathEditor)

makeDisplayInfoPrompt :: Tab -> Prompt
makeDisplayInfoPrompt = withSelectedEntry (DisplayInfo . entryInfo)

makeSearchPrompt :: Tab -> Prompt
makeSearchPrompt = withDirTabPath (Search emptyPathEditor)

withSelectedEntry :: (Entry -> PromptAction) -> Tab -> Prompt
withSelectedEntry func tab = Prompt tab $ case selectedEntry tab of
  Just entry -> func entry
  _ -> DisplayError "This tab does not have a selected entry"

withDirTabPath :: (FilePath -> PromptAction) -> Tab -> Prompt
withDirTabPath func tab = Prompt tab $ case tab of
   DirTab {tabPath = path} -> func path
   _ -> DisplayError "This tab does not represent a directory"

-- rendering functions
renderPrompt :: Prompt -> Widget Name
renderPrompt prompt = centerLayer . box $ vBox [body, hBorder, footer]
  where
    box = withDefAttr promptAttr . borderWithLabel (str . show $ action prompt) . hLimit 70
    body = padLeftRight 2 . padTopBottom 1 $ renderBody prompt
    footer = hCenter . renderFooter $ action prompt

renderBody :: Prompt -> Widget Name
renderBody pr = vBox $ case action pr of
  Copy en path -> disclaimer : map strWrap [tellEntry en <> " will be copied from:", takeDirectory $ entryPath en, "to: ", path]
  Cut en path -> disclaimer : map strWrap [tellEntry en <> " will be moved from:", takeDirectory $ entryPath en, "to: ", path]
  Rename edit en -> disclaimer : strWrap (tellEntry en <> " will be renamed to:") : renderValidatedEditor edit
  Delete en -> [disclaimer, strWrap $ tellEntry en <> " will be permanently deleted"]
  Mkdir edit _ -> str "Directory name:" : renderValidatedEditor edit
  Touch edit _ -> str "File name:" : renderValidatedEditor edit
  GoTo edit -> str "Directory to open:" : renderValidatedEditor edit
  Search edit _ -> str "Search for:" : renderValidatedEditor edit
  DisplayInfo info -> map strWrap . (displaySize info :) $ displayPerms info ++ displayTimes info
  DisplayError msg -> [str "Whoops, this went wrong:", withDefAttr errorAttr $ strWrap msg]

displaySize :: EntryInfo -> String
displaySize info = "Size: " ++ show (entrySize info) ++ " Bytes"

displayPerms :: EntryInfo -> [String]
displayPerms info = case entryPerms info of
  Nothing -> [" ", "Permissions unknown", "(could not read them)"]
  Just p -> [
      " ",
      "Is readable: " <> (if readable p then "yes" else "no"),
      "Is writable: " <> (if writable p then "yes" else "no"),
      "Is executable: " <> (if executable p then "yes" else "no"),
      "Is searchable: " <> (if searchable p then "yes" else "no")
    ]

displayTimes :: EntryInfo -> [String]
displayTimes info = case entryTimes info of
  Nothing -> [" ", "Last access and modification times unknown", "(could not read them)"]
  Just (acTm, mdTm) -> [" ", "Last access time:" <> format acTm, "Last modification time:" <> format mdTm]
    where format = formatTime defaultTimeLocale " %T %B %-d %Y"

tellEntry :: Entry -> String
tellEntry e = case e of
  DirEntry {entryName = name} -> "The directory " <> name <> " (and all it's content)"
  FileEntry {entryName = name} -> "The file " <> name

disclaimer :: Widget Name
disclaimer = withDefAttr disclaimerAttr $ strWrap "NOTE: this will operate on \
  \your file system and may be irreversible, double check it! Also please note \
  \that when this operation starts the UI will be unresponsive until it's done."

renderValidatedEditor :: PathEditor -> [Widget Name]
renderValidatedEditor e = [renderEditor (str . unlines) True e, validLine]
  where
    validLine = if isValid $ getEditLine e
      then str " "
      else withDefAttr errorAttr $ str " ^ invalid filepath!"

renderFooter :: PromptAction -> Widget Name
renderFooter act = kb "Enter" <+> str txt <+> kb "Esc" <+> str " to close and go back"
  where
    kb = withDefAttr keybindAttr . str
    txt = case act of
      Copy _ _ -> " to copy, "
      Cut _ _ -> " to move, "
      Rename _ _ -> " to rename, "
      Delete _ -> " to delete, "
      Mkdir _ _ -> " to make the directory, "
      Touch _ _ -> " to touch the file, "
      GoTo _ -> " to change directory, "
      Search _ _ -> " to search, "
      _ -> " or "

-- event-handling functions
handlePromptEvent :: Event -> Prompt -> EventM Name (Either Prompt Tab)
handlePromptEvent ev pr = case ev of
  EvKey KEsc [] -> return . Right $ originTab pr
  EvKey KEnter [] -> liftIO $ tryProcessAction pr
  _ -> Left . Prompt (originTab pr) <$> handleActionEditor ev (action pr)

tryProcessAction :: Prompt -> IO (Either Prompt Tab)
tryProcessAction pr = do
  result <- (try $ processAction pr) :: IO (Either SomeException Tab)
  return $ case result of
    Left e -> Left . Prompt (originTab pr) . DisplayError $ displayException e
    Right tabRes -> Right tabRes

processAction :: Prompt -> IO Tab
processAction Prompt {originTab = tab, action = act} = case act of
  Copy FileEntry {entryPath = ePath} path -> copyFileWithMetadata ePath (path </> takeFileName ePath) *> reload tab
  Copy DirEntry {entryPath = ePath} path -> copyDirectoryRecursive ePath (path </> takeFileName ePath) *> reload tab
  Cut FileEntry {entryPath = ePath} path -> moveFileWithMetadata ePath (path </> takeFileName ePath) *> reload tab
  Cut DirEntry {entryPath = ePath} path -> moveDirectoryRecursive ePath (path </> takeFileName ePath) *> reload tab
  Rename edit FileEntry {entryPath = ePath} -> renameFile ePath (takeDirectory ePath </> getEditLine edit) *> reload tab
  Rename edit DirEntry {entryPath = ePath} -> moveDirectoryRecursive ePath (takeDirectory ePath </> getEditLine edit) *> reload tab
  Delete FileEntry {entryPath = ePath} -> removeFile ePath *> reload tab
  Delete DirEntry {entryPath = ePath} -> removeDirectoryRecursive ePath *> reload tab
  Mkdir edit path -> createDirectory (path </> getEditLine edit) *> reload tab
  Touch edit path -> writeFile (path </> getEditLine edit) "" *> reload tab
  GoTo edit -> processGoTo $ getEditLine edit
  Search edit path -> makeSearchTab path $ getEditLine edit
  _ -> return tab

processGoTo :: FilePath -> IO Tab
processGoTo path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  if isFile || not isDir
    then throw . userError $ path <> " does not exist or is not a directory"
    else makeDirTab path

handleActionEditor :: Event -> PromptAction -> EventM Name PromptAction
handleActionEditor ev act = case act of
  Rename edit en -> (`Rename` en) <$> handleEditorEvent ev edit
  Mkdir edit path -> (`Mkdir` path) <$> handleEditorEvent ev edit
  Touch edit path -> (`Touch` path) <$> handleEditorEvent ev edit
  GoTo edit -> GoTo <$> handleEditorEvent ev edit
  Search edit path -> (`Search` path) <$> handleEditorEvent ev edit
  _ -> return act

-- utility functions
getEditLine :: PathEditor -> String
getEditLine = head . getEditContents

-- files functions not covered by System.Directory nor System.FilePath
moveFileWithMetadata :: FilePath -> FilePath -> IO ()
moveFileWithMetadata o d = do
  copyFileWithMetadata o d
  removeFile o

moveDirectoryRecursive :: FilePath -> FilePath -> IO ()
moveDirectoryRecursive o d = do
  copyDirectoryRecursive o d
  removeDirectoryRecursive o

-- taken from https://stackoverflow.com/questions/6807025/what-is-the-haskell-way-to-copy-a-directory
copyDirectoryRecursive ::  FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dst = do
  whenM (not <$> doesDirectoryExist src) $ throw (userError "source does not exist")
  whenM (doesFileOrDirectoryExist dst) $ throw (userError "destination already exists")
  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDirectoryRecursive srcPath dstPath
      else copyFileWithMetadata srcPath dstPath
  where
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
    orM xs = or <$> sequence xs
    whenM s r = s >>= flip when r
