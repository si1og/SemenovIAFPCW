module Logging
  ( LogLevel(..)
  , LogEntry(..)
  , initLogging
  , logEvent
  , formatEntry
  ) where

import Config.App (LogConfig)
import Config.App qualified as Config
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO

data LogLevel = Info | Warning | Error
  deriving (Eq, Ord, Show)

data LogEntry = LogEntry
  { logLevel :: LogLevel
  , logMessage :: Text
  }
  deriving (Eq, Show)

initLogging :: LogConfig -> IO ()
initLogging config =
  if Config.lcAppendMode config
    then pure ()
    else TextIO.writeFile (Config.lcLogFile config) Text.empty

logEvent :: LogConfig -> LogLevel -> Text -> IO ()
logEvent config level message =
  TextIO.appendFile (Config.lcLogFile config) (formatEntry (LogEntry level message) <> Text.pack "\n")

formatEntry :: LogEntry -> Text
formatEntry entry =
  Text.pack "["
    <> Text.pack (show (logLevel entry))
    <> Text.pack "] "
    <> logMessage entry
