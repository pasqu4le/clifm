module Main where
import Types
import Widgets.Manager

import Options.Applicative
import System.Directory (doesDirectoryExist, makeAbsolute)
import Data.Semigroup ((<>))
import Control.Monad (void)
import Brick.Main (customMain, showFirstCursor, App(..))
import Graphics.Vty (mkVty, standardIOConfig, setMode, outputIface, Mode(Mouse))

-- entry point: parses the arguments and starts the brick application

-- options
newtype FMOptions = FMOptions {dirPath :: FilePath}

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

main :: IO ()
main = startFM =<< execParser options
  where
    options = info (opts <**> helper)
      ( fullDesc
     <> header "Command Line Interface File Manager"
     <> progDesc "A simple CLI-based File Manager" )

startFM :: FMOptions -> IO ()
startFM (FMOptions path) = do
  isDir <- doesDirectoryExist path
  if isDir then makeAbsolute path >>= runUI
  else runUI []

-- brick application
runUI :: FilePath -> IO ()
runUI path = do
  let buildVty = do
        v <- mkVty =<< standardIOConfig
        setMode (outputIface v) Mouse True
        return v
  state <- makeState path
  void $ customMain buildVty Nothing app state

app :: App State e Name
app = App { appDraw = drawUi,
            appStartEvent = return,
            appHandleEvent = handleEvent,
            appAttrMap = const attributes,
            appChooseCursor = showFirstCursor
          }
