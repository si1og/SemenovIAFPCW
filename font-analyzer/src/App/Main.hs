module App.Main (main) where

import Config.App qualified as Config
import Input.CLI qualified as CLI
import Logging qualified

main :: IO ()
main = do
  let config = Config.defaultConfig
  Logging.initLogging (Config.acLogConfig config)
  CLI.runCLI config
