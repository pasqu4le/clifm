module Main where
import Types
import Widgets.Manager

import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute)
import Data.Semigroup ((<>))
import Control.Monad (void)
import Brick.Main (customMain, showFirstCursor, App(..))
import Brick.Themes (Theme, themeToAttrMap, loadCustomizations)
import Brick.AttrMap (AttrMap)
import Graphics.Vty (mkVty, standardIOConfig, setMode, outputIface, Mode(Mouse))

-- entry point: parses the arguments and starts the brick application

-- options
data FMOptions = FMOptions {dirPath :: FilePath, editComm :: String, themeType :: ThemeType}
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

customTheme :: Parser ThemeType
customTheme = CustomTheme <$> strOption
  (  long "theme"
  <> short 't'
  <> metavar "FILEPATH"
  <> help "Load a custom theme from an INI file" )

defTheme :: Parser ThemeType
defTheme = flag DefaultTheme DefaultTheme
  ( long "default-theme"
  <> short 'd'
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
  state <- makeState path $ editComm options
  void $ customMain buildVty Nothing (app atrm) state

app :: AttrMap -> App State e Name
app atrm = App { appDraw = drawUi,
    appStartEvent = return,
    appHandleEvent = handleEvent,
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
