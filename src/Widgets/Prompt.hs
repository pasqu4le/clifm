module Widgets.Prompt where
import Commons
import qualified Widgets.Tab as Tab
import qualified Widgets.Menu as Menu
import qualified Widgets.Entry as Entry

import Data.Monoid ((<>))
import Data.Functor (($>))
import Control.Monad(when,forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkFinally, ThreadId, killThread)
import Control.Exception (try, throw, displayException, SomeException, fromException)
import Control.Exception.Base (AsyncException(ThreadKilled))
import Control.Applicative ((*>), (<$>))
import Brick.Widgets.Core ((<+>), str, strWrap, vBox, hLimit, padLeftRight, padTopBottom, withDefAttr)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Types (Widget, EventM, BrickEvent(..))
import Brick.Widgets.Center (centerLayer, hCenter)
import qualified Brick.Widgets.Edit as Edit
import Brick.BChan (BChan, writeBChan)
import Graphics.Vty (Event(EvKey), Key(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.FilePath (isValid, takeDirectory, (</>), takeFileName)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory, renameFile,
  copyFileWithMetadata, removeFile, removeDirectoryRecursive, getDirectoryContents,
  readable, writable, executable, searchable)

data Prompt = Prompt {originTab :: Tab.Tab, originPane :: PaneName, action :: Action} deriving Show
type Editor = Edit.Editor FilePath Name
data Action = Copy Entry.Entry FilePath | Cut Entry.Entry FilePath | Rename Editor Entry.Entry |
  Delete Entry.Entry | Mkdir Editor FilePath | Touch Editor FilePath |
  GoTo Editor | Search Editor FilePath | DisplayInfo Entry.Info |
  DisplayError String | Performing String ThreadId

instance Show Action where
  show (Copy _ _) = " Copy "
  show (Cut _ _) = " Cut "
  show (Rename _ _) = " Rename "
  show (Delete _) = " Delete "
  show (Mkdir _ _) = " Make Directory "
  show (Touch _ _) = " Touch File "
  show (GoTo _) = " Go To "
  show (DisplayInfo _) = " Entry Info "
  show (Search _ _) = " Search "
  show (Performing name _) = " Performing" ++ name
  show _ = " Error "

-- creation functions
emptyEditor :: Editor
emptyEditor = makeEditor []

makeEditor :: FilePath -> Editor
makeEditor = Edit.editor PromptEditor (Just 1)

paste :: Menu.Clipboard -> Tab.Tab -> PaneName -> Prompt
paste c tab pName = Prompt tab pName $ case (c, tab) of
  (Menu.EmptyBoard, _) -> DisplayError "The clipboard is empty"
  (_, Tab.Empty) -> DisplayError "You cannot paste into an empty tab"
  (_, Tab.Search {}) -> DisplayError "You cannot paste into a search tab"
  (Menu.CopyBoard {Menu.fromEntry = entry}, Tab.Dir{Tab.path = path}) -> Copy entry path
  (Menu.CutBoard {Menu.fromEntry = entry}, Tab.Dir{Tab.path = path}) -> Cut entry path

goTo :: Tab.Tab -> PaneName -> Prompt
goTo tab pName = Prompt tab pName $ GoTo emptyEditor

rename :: Tab.Tab -> PaneName -> Prompt
rename = withSelectedEntry (\en -> Rename (editorFromEntry en) en)
  where editorFromEntry = makeEditor . takeFileName . Entry.path

delete :: Tab.Tab -> PaneName -> Prompt
delete = withSelectedEntry Delete

mkdir :: Tab.Tab -> PaneName -> Prompt
mkdir = withDirTabPath (Mkdir emptyEditor)

touch :: Tab.Tab -> PaneName -> Prompt
touch = withDirTabPath (Touch emptyEditor)

displayInfo :: Tab.Tab -> PaneName -> Prompt
displayInfo = withSelectedEntry (DisplayInfo . Entry.info)

search :: Tab.Tab -> PaneName -> Prompt
search = withDirTabPath (Search emptyEditor)

withSelectedEntry :: (Entry.Entry -> Action) -> Tab.Tab -> PaneName -> Prompt
withSelectedEntry func tab pName = Prompt tab pName $ case Tab.selectedEntry tab of
  Just entry -> func entry
  _ -> DisplayError "This tab does not have a selected entry"

withDirTabPath :: (FilePath -> Action) -> Tab.Tab -> PaneName -> Prompt
withDirTabPath func tab pName = Prompt tab pName $ case tab of
   Tab.Dir {Tab.path = path} -> func path
   _ -> DisplayError "This tab does not represent a directory"

-- rendering functions
render :: Prompt -> Widget Name
render prompt = centerLayer . box $ vBox [body, hBorder, footer]
  where
    box = withDefAttr promptAttr . borderWithLabel (str . show $ action prompt) . hLimit 70
    body = padLeftRight 2 . padTopBottom 1 $ renderBody prompt
    footer = hCenter . renderFooter $ action prompt

renderBody :: Prompt -> Widget Name
renderBody pr = vBox $ case action pr of
  Copy en path -> disclaimer : map strWrap [tellEntry en <> " will be copied from:", takeDirectory $ Entry.path en, "to: ", path]
  Cut en path -> disclaimer : map strWrap [tellEntry en <> " will be moved from:", takeDirectory $ Entry.path en, "to: ", path]
  Rename edit en -> disclaimer : strWrap (tellEntry en <> " will be renamed to:") : renderValidatedEditor edit
  Delete en -> [disclaimer, strWrap $ tellEntry en <> " will be permanently deleted"]
  Mkdir edit _ -> str "Directory name:" : renderValidatedEditor edit
  Touch edit _ -> str "File name:" : renderValidatedEditor edit
  GoTo edit -> str "Directory to open:" : renderValidatedEditor edit
  Search edit _ -> str "Search for:" : renderValidatedEditor edit
  DisplayInfo info -> map strWrap . (displaySize info :) $ displayPerms info ++ displayTimes info
  DisplayError msg -> [str "Whoops, this went wrong:", withDefAttr errorAttr $ strWrap msg]
  Performing name _ -> [str $ "Performing" ++ name, str "Please wait"]

displaySize :: Entry.Info -> String
displaySize info = case Entry.size info of
  Just s -> "Size: " ++ show s ++ " Bytes (" ++ Entry.shortSize info ++ ")"
  _ -> "Size unknown (unable to read)"


displayPerms :: Entry.Info -> [String]
displayPerms info = case Entry.perms info of
  Nothing -> [" ", "Permissions unknown", "(could not read them)"]
  Just p -> [
      " ",
      "Is readable: " <> (if readable p then "yes" else "no"),
      "Is writable: " <> (if writable p then "yes" else "no"),
      "Is executable: " <> (if executable p then "yes" else "no"),
      "Is searchable: " <> (if searchable p then "yes" else "no")
    ]

displayTimes :: Entry.Info -> [String]
displayTimes info = case Entry.times info of
  Nothing -> [" ", "Last access and modification times unknown", "(could not read them)"]
  Just (acTm, mdTm) -> [" ", "Last access time:" <> format acTm, "Last modification time:" <> format mdTm]
    where format = formatTime defaultTimeLocale " %T %B %-d %Y"

tellEntry :: Entry.Entry -> String
tellEntry e = case e of
  Entry.Dir {Entry.name = name} -> "The directory " <> name <> " (and all it's content)"
  Entry.File {Entry.name = name} -> "The file " <> name

disclaimer :: Widget Name
disclaimer = withDefAttr disclaimerAttr $ strWrap "NOTE: this will operate on \
  \your file system and may be irreversible, double check it! Also please note \
  \that the operation can be stopped, but will not revert what was already done."

renderValidatedEditor :: Editor -> [Widget Name]
renderValidatedEditor e = [Edit.renderEditor (str . unlines) True e, validLine]
  where
    validLine = if isValid $ editorLine e
      then str " "
      else withDefAttr errorAttr $ str " ^ invalid filepath!"

renderFooter :: Action -> Widget Name
renderFooter act = case act of
  Performing _ _ -> kb "Esc" <+> str " to Cancel. NOTE: will not revert what was already done."
  _ -> kb "Enter" <+> str txt <+> kb "Esc" <+> str " to close and go back"
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
handleEvent :: BrickEvent Name (ThreadEvent Tab.Tab) -> Prompt -> BChan (ThreadEvent Tab.Tab) -> EventM Name (Either Prompt Tab.Tab)
handleEvent (AppEvent ev) pr _ = case ev of
  ThreadError err -> return $ Left pr {action = DisplayError err}
  ThreadSuccess tab -> return $ Right tab
  ThreadClosed -> return . Right $ originTab pr
handleEvent (VtyEvent ev) pr eChan = case ev of
  EvKey KEsc [] -> liftIO $ exit pr
  EvKey KEnter [] -> liftIO $ performAction pr eChan
  _ -> Left . Prompt (originTab pr) (originPane pr) <$> handleEditor ev (action pr)
handleEvent _ pr _ = return $ Left pr

exit :: Prompt -> IO (Either Prompt Tab.Tab)
exit pr = case action pr of
  Performing name tId -> killThread tId $> Left pr -- returns the same prompt because the actual exiting will happen because of the exception that killThread raises
  _ -> return . Right $ originTab pr

-- gets to decide if the action will be processed in a different thread or not
performAction :: Prompt -> BChan (ThreadEvent Tab.Tab) -> IO (Either Prompt Tab.Tab) --
performAction pr eChan = case action pr of
  Copy _ _ -> Left <$> processThreaded pr eChan
  Cut _ _ -> Left <$> processThreaded pr eChan
  Rename _ _ -> Left <$> processThreaded pr eChan
  Delete _ -> Left <$> processThreaded pr eChan
  Search _ _ -> Left <$> processThreaded pr eChan
  Performing _ _ -> return $ Left pr -- doesn't really make sense
  _ -> processSafe pr

processThreaded :: Prompt -> BChan (ThreadEvent Tab.Tab) -> IO Prompt
processThreaded pr eChan = do
  tId <- forkFinally (processUnsafe pr) (reportResult eChan)
  return $ pr {action = Performing (show $ action pr) tId}

reportResult :: BChan (ThreadEvent Tab.Tab) -> Either SomeException Tab.Tab -> IO ()
reportResult eChan res = writeBChan eChan $ case res of
  Left e -> endingEvent e
  Right tabRes -> ThreadSuccess tabRes

endingEvent :: SomeException -> ThreadEvent Tab.Tab
endingEvent e = case (fromException e :: Maybe AsyncException) of
  Just ThreadKilled -> ThreadClosed
  _ -> ThreadError $ displayException e

processSafe :: Prompt -> IO (Either Prompt Tab.Tab)
processSafe pr = do
  result <- (try $ processUnsafe pr) :: IO (Either SomeException Tab.Tab)
  return $ case result of
    Left e -> Left $ pr {action = DisplayError $ displayException e}
    Right tabRes -> Right tabRes

processUnsafe :: Prompt -> IO Tab.Tab
processUnsafe Prompt {originTab = tab, originPane = pName, action = act} = case act of
  Copy Entry.File {Entry.path = ePath} path -> copyFileWithMetadata ePath (path </> takeFileName ePath) *> Tab.reload pName tab
  Copy Entry.Dir {Entry.path = ePath} path -> copyDirectoryRecursive ePath (path </> takeFileName ePath) *> Tab.reload pName tab
  Cut Entry.File {Entry.path = ePath} path -> moveFileWithMetadata ePath (path </> takeFileName ePath) *> Tab.reload pName tab
  Cut Entry.Dir {Entry.path = ePath} path -> moveDirectoryRecursive ePath (path </> takeFileName ePath) *> Tab.reload pName tab
  Rename edit Entry.File {Entry.path = ePath} -> renameFile ePath (takeDirectory ePath </> editorLine edit) *> Tab.reload pName tab
  Rename edit Entry.Dir {Entry.path = ePath} -> moveDirectoryRecursive ePath (takeDirectory ePath </> editorLine edit) *> Tab.reload pName tab
  Delete Entry.File {Entry.path = ePath} -> removeFile ePath *> Tab.reload pName tab
  Delete Entry.Dir {Entry.path = ePath} -> removeDirectoryRecursive ePath *> Tab.reload pName tab
  Mkdir edit path -> createDirectory (path </> editorLine edit) *> Tab.reload pName tab
  Touch edit path -> writeFile (path </> editorLine edit) "" *> Tab.reload pName tab
  GoTo edit -> processGoTo pName $ editorLine edit
  Search edit path -> Tab.makeSearchTab pName path $ editorLine edit
  _ -> return tab

processGoTo :: PaneName -> FilePath -> IO Tab.Tab
processGoTo pName path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  if isFile || not isDir
    then throw . userError $ path <> " does not exist or is not a directory"
    else Tab.makeDirTab pName path

handleEditor :: Event -> Action -> EventM Name Action
handleEditor ev act = case act of
  Rename edit en -> (`Rename` en) <$> Edit.handleEditorEvent ev edit
  Mkdir edit path -> (`Mkdir` path) <$> Edit.handleEditorEvent ev edit
  Touch edit path -> (`Touch` path) <$> Edit.handleEditorEvent ev edit
  GoTo edit -> GoTo <$> Edit.handleEditorEvent ev edit
  Search edit path -> (`Search` path) <$> Edit.handleEditorEvent ev edit
  _ -> return act

-- utility functions
editorLine :: Editor -> String
editorLine = head . Edit.getEditContents

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
