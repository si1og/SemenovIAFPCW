module Report
  ( assembleReport
  , formatReportText
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Domain.Types

assembleReport
  :: BDFFont
  -> FontMetrics
  -> [Anomaly]
  -> [ReplacementSuggestion]
  -> AnalysisReport
assembleReport font metrics anomalies replacements =
  AnalysisReport
    { reportFont = font
    , reportMetrics = metrics
    , reportAnomalies = anomalies
    , reportReplacements = replacements
    }

formatReportText :: AnalysisReport -> Text
formatReportText _report = Text.pack "Analysis report is not implemented yet."
