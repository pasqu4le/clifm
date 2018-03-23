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
import System.FilePath (isValid, takeDirectory, (</>), takeFileName)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory, renameFile,
  copyFileWithMetadata, renameFile, removeFile, removeDirectoryRecursive, getDirectoryContents)

data Prompt = Prompt {originTab :: Tab, action :: PromptAction} deriving Show
type PathEditor = Editor FilePath Name
data PromptAction = Copy Entry FilePath | Cut Entry FilePath | Rename PathEditor Entry |
  Delete Entry | Mkdir PathEditor FilePath | Touch PathEditor FilePath |
  GoTo PathEditor | DisplayError String

instance Show PromptAction where
  show (Copy _ _) = " Copy "
  show (Cut _ _) = " Cut "
  show (Rename _ _) = " Rename "
  show (Delete _) = " Delete "
  show (Mkdir _ _) = " Make Directory "
  show (Touch _ _) = " Touch File "
  show (GoTo _) = " Go To "
  show _ = " Error "

-- creation functions
emptyPathEditor :: PathEditor
emptyPathEditor = makePathEditor []

makePathEditor :: FilePath -> PathEditor
makePathEditor = editor PEdit (Just 1)

makePastePrompt :: Clipboard -> Tab -> Prompt
makePastePrompt c tab = Prompt tab $ case (c, maybeTabPath tab) of
  (_, Nothing) -> DisplayError "The current tab is not a directory"
  (EmptyBoard, _) -> DisplayError "The clipboard is empty"
  (CopyBoard entry, Just path) -> Copy entry path
  (CutBoard entry, Just path) -> Cut entry path

makeGoToPrompt :: Tab -> Prompt
makeGoToPrompt tab = Prompt tab $ GoTo emptyPathEditor

makeRenamePrompt :: Tab -> Prompt
makeRenamePrompt = withSelectedEntry (\en -> Rename (editorFromEntry en) en)
  where editorFromEntry = makePathEditor . takeFileName . entryPath

makeDeletePrompt :: Tab -> Prompt
makeDeletePrompt = withSelectedEntry Delete

makeMkdirPrompt :: Tab -> Prompt
makeMkdirPrompt = withTabPath (Mkdir emptyPathEditor)

makeTouchPrompt :: Tab -> Prompt
makeTouchPrompt = withTabPath (Touch emptyPathEditor)

withSelectedEntry :: (Entry -> PromptAction) -> Tab -> Prompt
withSelectedEntry func tab = Prompt tab $ case selectedEntry tab of
  Just entry -> func entry
  _ -> DisplayError "This tab does not have a selected entry"

withTabPath :: (FilePath -> PromptAction) -> Tab -> Prompt
withTabPath func tab = Prompt tab $ case maybeTabPath tab of
   Just path -> func path
   _ -> DisplayError "This tab does not represent a directory"

-- rendering functions
renderPrompt :: Prompt -> Widget Name
renderPrompt (Prompt tab act) = centerLayer . box $ vBox [body, hBorder, footer]
  where
    box = withDefAttr promptAttr . borderWithLabel (str $ show act) . hLimit 70
    body = padLeftRight 2 . padTopBottom 1 $ renderBody (Prompt tab act)
    footer = hCenter $ renderFooter act

renderBody :: Prompt -> Widget Name
renderBody pr = vBox $ case action pr of
  Copy en path -> disclaimer : map strWrap [tellEntry en <> " will be copied from:", takeDirectory $ entryPath en, "to: ", path]
  Cut en path -> disclaimer : map strWrap [tellEntry en <> " will be moved from:", takeDirectory $ entryPath en, "to: ", path]
  Rename edit en -> disclaimer : strWrap (tellEntry en <> " will be renamed to:") : renderValidatedEditor edit
  Delete en -> [disclaimer, strWrap $ tellEntry en <> " will be permanently deleted"]
  Mkdir edit _ -> str "Directory name:" : renderValidatedEditor edit
  Touch edit _ -> str "File name:" : renderValidatedEditor edit
  GoTo edit -> str "Directory to open:" : renderValidatedEditor edit
  DisplayError msg -> [str "Whoops, this went wrong:", withDefAttr errorAttr $ strWrap msg]

tellEntry :: Entry -> String
tellEntry e = case e of
  DirEntry name _ -> "The directory " <> name <> " (and all it's content)"
  FileEntry name _ _ -> "The file " <> name

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
      _ -> " or "

-- event-handling functions
handlePromptEvent :: Event -> Prompt -> EventM Name (Either Prompt Tab)
handlePromptEvent ev (Prompt tab act) = case ev of
  EvKey KEsc [] -> return $ Right tab
  EvKey KEnter [] -> liftIO $ tryProcessAction (Prompt tab act)
  _ -> (Left . Prompt tab) <$> handleActionEditor ev act

tryProcessAction :: Prompt -> IO (Either Prompt Tab)
tryProcessAction pr = do
  result <- (try $ processAction pr) :: IO (Either SomeException Tab)
  return $ case result of
    Left e -> Left . Prompt (originTab pr) . DisplayError $ displayException e
    Right tabRes -> Right tabRes

processAction :: Prompt -> IO Tab
processAction (Prompt tab act) = case act of
  Copy (FileEntry _ fPath _) path -> copyFileWithMetadata fPath (path </> takeFileName fPath) *> reload tab
  Copy (DirEntry _ dPath) path -> copyDirectoryRecursive dPath (path </> takeFileName dPath) *> reload tab
  Cut (FileEntry _ fPath _) path -> moveFileWithMetadata fPath (path </> takeFileName fPath) *> reload tab
  Cut (DirEntry _ dPath) path -> moveDirectoryRecursive dPath (path </> takeFileName dPath) *> reload tab
  Rename edit (FileEntry _ fPath _) -> renameFile fPath (takeDirectory fPath </> getEditLine edit) *> reload tab
  Rename edit (DirEntry _ dPath) -> moveDirectoryRecursive dPath (takeDirectory dPath </> getEditLine edit) *> reload tab
  Delete (FileEntry _ path _) -> removeFile path *> reload tab
  Delete (DirEntry _ path) -> removeDirectoryRecursive path *> reload tab
  Mkdir edit path -> createDirectory (path </> getEditLine edit) *> reload tab
  Touch edit path -> writeFile (path </> getEditLine edit) "" *> reload tab
  GoTo edit -> processGoTo $ getEditLine edit
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
