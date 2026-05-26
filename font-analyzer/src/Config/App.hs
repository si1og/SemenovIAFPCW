module Config.App
  ( AppConfig(..)
  , LogConfig(..)
  , DetectionThresholds(..)
  , AnalysisOptions(..)
  , defaultConfig
  , defaultLogConfig
  , defaultThresholds
  , defaultAnalysisOptions
  ) where

import Domain.Types
  ( DetectionThresholds(..)
  )

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

data AnalysisOptions = AnalysisOptions
  { aoAnalyzeReadability :: Bool
  , aoAnalyzeProportion :: Bool
  , aoAnalyzeDensity :: Bool
  , aoAnalyzeDistinctness :: Bool
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
    , dtMinDistinctness = 0.016
    , dtMinDensity = 0.05
    , dtMaxDensity = 0.85
    }

defaultAnalysisOptions :: AnalysisOptions
defaultAnalysisOptions =
  AnalysisOptions
    { aoAnalyzeReadability = True
    , aoAnalyzeProportion = True
    , aoAnalyzeDensity = True
    , aoAnalyzeDistinctness = True
    }
