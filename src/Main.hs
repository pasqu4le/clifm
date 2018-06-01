module Main where
import Commons
import qualified Widgets.Manager as Mngr
import qualified Widgets.Tab as Tab

import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute)
import Data.Semigroup ((<>))
import Control.Monad (void)
import Brick.Main (customMain, showFirstCursor, App(..))
import Brick.Themes (Theme, themeToAttrMap, loadCustomizations)
import Brick.AttrMap (AttrMap)
import Brick.BChan (newBChan)
import Graphics.Vty (mkVty, standardIOConfig, setMode, outputIface, Mode(Mouse))

-- entry point: parses the arguments and starts the brick application

-- options
data FMOptions = FMOptions {dirPath :: FilePath, editComm :: String, themeType :: ThemeType, threadNum :: Int}
data ThemeType = CustomTheme FilePath | DefaultTheme

-- argument parsing functions
opts :: Parser FMOptions
opts = FMOptions
  <$> strOption
    ( long "dir-path"
    <> short 'd'
    <> metavar "FILEPATH"
    <> help "Directory to open"
    <> showDefault
    <> value ".")
  <*> strOption
    ( long "editor"
    <> short 'e'
    <> help "Editor command/path (file path will be appended to this)"
    <> showDefault
    <> value "nano")
  <*> (customTheme <|> defTheme)
  <*> option auto
    ( long "thread-num"
    <> short 'n'
    <> help "Max number of size-searching threads"
    <> showDefault
    <> value 4
    <> metavar "INT")


customTheme :: Parser ThemeType
customTheme = CustomTheme <$> strOption
  (  long "theme"
  <> short 't'
  <> metavar "FILEPATH"
  <> help "Load a custom theme from an INI file" )

defTheme :: Parser ThemeType
defTheme = flag DefaultTheme DefaultTheme
  ( long "default-theme"
  <> help "Use the default theme" )

main :: IO ()
main = runUI =<< execParser options
  where
    options = info (opts <**> helper)
      ( fullDesc
     <> header "Command Line Interface File Manager"
     <> progDesc "A simple CLI-based File Manager" )

runUI :: FMOptions -> IO ()
runUI options = do
  isDir <- doesDirectoryExist $ dirPath options
  path <- if isDir then makeAbsolute $ dirPath options else return []
  theme <- loadTheme $ themeType options
  let atrm = themeToAttrMap theme
      buildVty = do
        v <- mkVty =<< standardIOConfig
        setMode (outputIface v) Mouse True
        return v
  eventChan <- Brick.BChan.newBChan 10
  state <- Mngr.makeState path (editComm options) eventChan (threadNum options)
  void $ customMain buildVty (Just eventChan) (app atrm) state

app :: AttrMap -> App Mngr.State (ThreadEvent Tab.Tab) Name
app atrm = App {
    appDraw = Mngr.drawUi,
    appStartEvent = return,
    appHandleEvent = Mngr.handleEvent,
    appAttrMap = const atrm,
    appChooseCursor = showFirstCursor
  }

loadTheme :: ThemeType -> IO Theme
loadTheme selTheme = case selTheme of
  DefaultTheme -> return defaultTheme
  CustomTheme path -> do
    isFile <- doesFileExist path
    if not isFile then return defaultTheme
    else do
      customTheme <- loadCustomizations path defaultTheme
      return $ case customTheme of
        Right theme -> theme
        Left _ -> defaultTheme
