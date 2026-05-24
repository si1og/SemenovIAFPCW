module App.Main (main) where

import qualified Config.App as Config
import qualified Input.CLI as CLI
import qualified Logging

main :: IO ()
main = do
  let config = Config.defaultConfig
  Logging.initLogging (Config.acLogConfig config)
  CLI.runCLI config
