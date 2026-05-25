module Config.App
  ( AppConfig(..)
  , LogConfig(..)
  , DetectionThresholds(..)
  , defaultConfig
  , defaultLogConfig
  , defaultThresholds
  ) where

data AppConfig = AppConfig
  { acReportOutputPath :: FilePath
  , acLogConfig :: LogConfig
  , acThresholds :: DetectionThresholds
  , acReferenceFontPath :: FilePath
  }
  deriving (Eq, Show)

data LogConfig = LogConfig
  { lcLogFile :: FilePath
  , lcMinLevel :: String
  , lcAppendMode :: Bool
  }
  deriving (Eq, Show)

data DetectionThresholds = DetectionThresholds
  { dtMinReadability :: Double
  , dtMinDistinctness :: Double
  , dtMinDensity :: Double
  , dtMaxDensity :: Double
  }
  deriving (Eq, Show)

defaultConfig :: AppConfig
defaultConfig =
  AppConfig
    { acReportOutputPath = "report.txt"
    , acLogConfig = defaultLogConfig
    , acThresholds = defaultThresholds
    , acReferenceFontPath = "data/Terminus 16v.bdf"
    }

defaultLogConfig :: LogConfig
defaultLogConfig =
  LogConfig
    { lcLogFile = "coursework.log"
    , lcMinLevel = "Info"
    , lcAppendMode = True
    }

defaultThresholds :: DetectionThresholds
defaultThresholds =
  DetectionThresholds
    { dtMinReadability = 0.4
    , dtMinDistinctness = 0.4
    , dtMinDensity = 0.05
    , dtMaxDensity = 0.85
    }
