module Widgets.Prompt where
import Commons
import qualified Widgets.Tab as Tab
import qualified Widgets.Menu as Menu
import qualified Widgets.Entry as Entry

import Control.Lens
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

data Prompt = Prompt {_originTab :: Tab.Tab, _originPane :: PaneName, _action :: Action} deriving Show
type Editor = Edit.Editor FilePath Name
data Action = Copy Entry.Entry FilePath | Cut Entry.Entry FilePath | Rename Editor Entry.Entry |
  Delete Entry.Entry | Mkdir Editor FilePath | Touch Editor FilePath |
  GoTo Editor | Search Editor FilePath | DisplayInfo Entry.Entry |
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

-- lenses
originTab :: Lens' Prompt Tab.Tab
originTab = lens _originTab (\prompt x -> prompt {_originTab = x})

originPane :: Lens' Prompt PaneName
originPane = lens _originPane (\prompt x -> prompt {_originPane = x})

action :: Lens' Prompt Action
action = lens _action (\prompt x -> prompt {_action = x})

-- creation
emptyEditor :: Editor
emptyEditor = makeEditor []

makeEditor :: FilePath -> Editor
makeEditor = Edit.editor PromptEditor (Just 1)

paste :: Menu.Clipboard -> Tab.Tab -> PaneName -> Prompt
paste clip tab pName = let
  checked
    | Menu.isEmpty clip = DisplayError "The clipboard is empty"
    | Tab.isEmpty tab = DisplayError "You cannot paste into an empty tab"
    | Tab.isSearch tab = DisplayError "You cannot paste into a search tab"
    | Menu.isCopy clip = Copy (clip ^. Menu.fromEntry) (tab ^. Tab.path)
    | Menu.isCut clip = Cut (clip ^. Menu.fromEntry) (tab ^. Tab.path)
    | otherwise = DisplayError "Cannot paste: unknown error"
  in Prompt tab pName checked

goTo :: Tab.Tab -> PaneName -> Prompt
goTo tab pName = Prompt tab pName $ GoTo emptyEditor

rename :: Tab.Tab -> PaneName -> Prompt
rename = withSelectedEntry (\en -> Rename (editorFromEntry en) en)
  where editorFromEntry = makeEditor . takeFileName . view Entry.path

delete :: Tab.Tab -> PaneName -> Prompt
delete = withSelectedEntry Delete

mkdir :: Tab.Tab -> PaneName -> Prompt
mkdir = withDirTabPath (Mkdir emptyEditor)

touch :: Tab.Tab -> PaneName -> Prompt
touch = withDirTabPath (Touch emptyEditor)

displayInfo :: Tab.Tab -> PaneName -> Prompt
displayInfo = withSelectedEntry DisplayInfo

search :: Tab.Tab -> PaneName -> Prompt
search = withDirTabPath (Search emptyEditor)

withSelectedEntry :: (Entry.Entry -> Action) -> Tab.Tab -> PaneName -> Prompt
withSelectedEntry func tab pName = Prompt tab pName $ case Tab.selectedEntry tab of
  Just entry -> func entry
  _ -> DisplayError "This tab does not have a selected entry"

withDirTabPath :: (FilePath -> Action) -> Tab.Tab -> PaneName -> Prompt
withDirTabPath func tab pName = Prompt tab pName $ if Tab.isDir tab
  then func $ view Tab.path tab
  else DisplayError "This tab does not represent a directory"

-- rendering
render :: Prompt -> Widget Name
render prompt = centerLayer . box $ vBox [body, hBorder, footer]
  where
    box = withDefAttr promptAttr . borderWithLabel (str . show $ view action prompt) . hLimit 70
    body = padLeftRight 2 . padTopBottom 1 $ renderBody prompt
    footer = hCenter . renderFooter $ view action prompt

renderBody :: Prompt -> Widget Name
renderBody pr = vBox $ case view action pr of
  Copy en path -> disclaimer : map strWrap [tellEntry en <> " will be copied from:", takeDirectory $ view Entry.path en, "to: ", path]
  Cut en path -> disclaimer : map strWrap [tellEntry en <> " will be moved from:", takeDirectory $ view Entry.path en, "to: ", path]
  Rename edit en -> disclaimer : strWrap (tellEntry en <> " will be renamed to:") : renderValidatedEditor edit
  Delete en -> [disclaimer, strWrap $ tellEntry en <> " will be permanently deleted"]
  Mkdir edit _ -> str "Directory name:" : renderValidatedEditor edit
  Touch edit _ -> str "File name:" : renderValidatedEditor edit
  GoTo edit -> str "Directory to open:" : renderValidatedEditor edit
  Search edit _ -> str "Search for:" : renderValidatedEditor edit
  DisplayInfo en -> map strWrap . (displaySize en :) $ displayPerms en ++ displayTimes en
  DisplayError msg -> [str "Whoops, this went wrong:", withDefAttr errorAttr $ strWrap msg]
  Performing name _ -> [str $ "Performing" ++ name, str "Please wait"]

displaySize :: Entry.Entry -> String
displaySize entry = case view Entry.size entry of
  Entry.Known s -> "Size: " ++ show s ++ " Bytes (" ++ Entry.shortSize (Entry.Known s) ++ ")"
  Entry.Unknown -> "Size unknown (unable to read)"
  Entry.Calculating -> "Size is being calculated"
  Entry.Waiting -> "Size will soon be calculated"
  _ -> ""

displayPerms :: Entry.Entry -> [String]
displayPerms entry = case view Entry.perms entry of
  Nothing -> [" ", "Permissions unknown", "(could not read them)"]
  Just p -> [
      " ",
      "Is readable: " <> (if readable p then "yes" else "no"),
      "Is writable: " <> (if writable p then "yes" else "no"),
      "Is executable: " <> (if executable p then "yes" else "no"),
      "Is searchable: " <> (if searchable p then "yes" else "no")
    ]

displayTimes :: Entry.Entry -> [String]
displayTimes entry = case view Entry.times entry of
  Nothing -> [" ", "Last access and modification times unknown", "(could not read them)"]
  Just (acTm, mdTm) -> [" ", "Last access time:" <> format acTm, "Last modification time:" <> format mdTm]
    where format = formatTime defaultTimeLocale " %T %B %-d %Y"

tellEntry :: Entry.Entry -> String
tellEntry entry
  | Entry.isDir entry = "The directory " <> name <> " (and all it's content)"
  | otherwise = "The file " <> name
  where name = view Entry.name entry

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

-- event-handling
handleEvent :: BrickEvent Name (ThreadEvent Tab.Tab) -> Prompt -> EChan Tab.Tab -> EventM Name (Either Prompt Tab.Tab)
handleEvent (AppEvent ev) pr _ = case ev of
  PromptError err -> return . Left $ pr & action .~ DisplayError err
  PromptSuccess tab -> return $ Right tab
  PromptClosed -> return . Right $ view originTab pr
  _ -> return $ Left pr
handleEvent (VtyEvent ev) pr eChan = case ev of
  EvKey KEsc [] -> liftIO $ exit pr
  EvKey KEnter [] -> liftIO $ performAction pr eChan
  _ -> Left <$> (pr & action %%~ handleEditor ev)
handleEvent _ pr _ = return $ Left pr

 -- if performing it will return the same prompt, the raised exception will trigger the closing
exit :: Prompt -> IO (Either Prompt Tab.Tab)
exit pr = case view action pr of
  Performing name tId -> killThread tId $> Left pr
  _ -> return . Right $ view originTab pr

-- gets to decide if the action will be processed in a different thread or not
performAction :: Prompt -> EChan Tab.Tab -> IO (Either Prompt Tab.Tab) --
performAction pr eChan = case view action pr of
  Copy _ _ -> Left <$> processThreaded pr eChan
  Cut _ _ -> Left <$> processThreaded pr eChan
  Rename _ _ -> Left <$> processThreaded pr eChan
  Delete _ -> Left <$> processThreaded pr eChan
  Search _ _ -> Left <$> processThreaded pr eChan
  Performing _ _ -> return $ Left pr -- doesn't really make sense
  _ -> processSafe pr

processThreaded :: Prompt -> EChan Tab.Tab -> IO Prompt
processThreaded pr eChan = do
  tId <- forkFinally (processUnsafe pr) (reportResult eChan)
  return $ pr & action %~ (\act -> Performing (show act) tId)

reportResult :: EChan Tab.Tab -> Either SomeException Tab.Tab -> IO ()
reportResult eChan = writeBChan eChan . either endingEvent PromptSuccess

endingEvent :: SomeException -> ThreadEvent Tab.Tab
endingEvent e = case (fromException e :: Maybe AsyncException) of
  Just ThreadKilled -> PromptClosed
  _ -> PromptError $ displayException e

processSafe :: Prompt -> IO (Either Prompt Tab.Tab)
processSafe pr = do
  result <- (try $ processUnsafe pr) :: IO (Either SomeException Tab.Tab)
  return $ case result of
    Left e -> Left $ pr & action .~ DisplayError (displayException e)
    Right tabRes -> Right tabRes

processUnsafe :: Prompt -> IO Tab.Tab
processUnsafe prompt = case act of
  Copy entry path -> processCopy entry path pName tab
  Cut entry path -> processCut entry path pName tab
  Rename edit entry -> processRename entry (editorLine edit) pName tab
  Delete entry -> processDelete entry pName tab
  Mkdir edit path -> createDirectory (path </> editorLine edit) *> Tab.reload pName tab
  Touch edit path -> writeFile (path </> editorLine edit) "" *> Tab.reload pName tab
  GoTo edit -> processGoTo pName $ editorLine edit
  Search edit path -> Tab.makeSearchTab pName path $ editorLine edit
  _ -> return tab
  where
    tab = view originTab prompt
    pName = view originPane prompt
    act = view action prompt

processCopy :: Entry.Entry -> FilePath -> PaneName -> Tab.Tab -> IO Tab.Tab
processCopy entry path pName tab
  | Entry.isFile entry = copyFileWithMetadata ePath (path </> takeFileName ePath) *> Tab.reload pName tab
  | otherwise = copyDirectoryRecursive ePath (path </> takeFileName ePath) *> Tab.reload pName tab
  where ePath = view Entry.path entry

processCut :: Entry.Entry -> FilePath -> PaneName -> Tab.Tab -> IO Tab.Tab
processCut entry path pName tab
  | Entry.isFile entry = moveFileWithMetadata ePath (path </> takeFileName ePath) *> Tab.reload pName tab
  | otherwise = moveDirectoryRecursive ePath (path </> takeFileName ePath) *> Tab.reload pName tab
  where ePath = view Entry.path entry

processRename :: Entry.Entry -> String -> PaneName -> Tab.Tab -> IO Tab.Tab
processRename entry editLine pName tab
  | Entry.isFile entry = renameFile ePath (takeDirectory ePath </> editLine) *> Tab.reload pName tab
  | otherwise = moveDirectoryRecursive ePath (takeDirectory ePath </> editLine) *> Tab.reload pName tab
  where ePath = view Entry.path entry 

processDelete :: Entry.Entry -> PaneName -> Tab.Tab -> IO Tab.Tab
processDelete entry pName tab
  | Entry.isFile entry = removeFile ePath *> Tab.reload pName tab
  | otherwise = removeDirectoryRecursive ePath *> Tab.reload pName tab
  where ePath = view Entry.path entry  

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

-- utility
editorLine :: Editor -> String
editorLine = head . Edit.getEditContents

notifySize :: FilePath -> Entry.Size -> Prompt -> Prompt
notifySize path size prompt = prompt & action %~ notifyActionSize path size

notifyActionSize :: FilePath -> Entry.Size -> Action -> Action
notifyActionSize path size act = case act of
  DisplayInfo en -> if path == en ^. Entry.path then DisplayInfo (en & Entry.size .~ size) else act
  _ -> act

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
