module Logging
  ( LogLevel(..)
  , LogEntry(..)
  , initLogging
  , logEvent
  , formatEntry
  ) where

import Config.App (LogConfig)
import Data.Text (Text)

data LogLevel = Info | Warning | Error
  deriving (Eq, Ord, Show)

data LogEntry = LogEntry
  { logLevel :: LogLevel
  , logMessage :: Text
  }
  deriving (Eq, Show)

initLogging :: LogConfig -> IO ()
initLogging _config = pure ()

logEvent :: LogConfig -> LogLevel -> Text -> IO ()
logEvent _config _level _message = pure ()

formatEntry :: LogEntry -> Text
formatEntry = logMessage
