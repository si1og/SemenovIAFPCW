module Input.CLI
  ( runCLI
  , askBDFPath
  , askSavePath
  , runAnalysisFlow
  ) where

import Config.App (AppConfig)

runCLI :: AppConfig -> IO ()
runCLI _config = putStrLn "BDF analyzer CLI is not implemented yet."

askBDFPath :: IO FilePath
askBDFPath = getLine

askSavePath :: IO (Maybe FilePath)
askSavePath = do
  path <- getLine
  pure $ if null path then Nothing else Just path

runAnalysisFlow :: AppConfig -> FilePath -> Maybe FilePath -> IO ()
runAnalysisFlow _config _inputPath _outputPath = pure ()
