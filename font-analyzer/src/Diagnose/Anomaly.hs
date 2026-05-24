module Diagnose.Anomaly
  ( detectAnomalies
  , classifyAnomaly
  , isAnomalous
  ) where

import Config.App (DetectionThresholds)
import Domain.Types

detectAnomalies :: DetectionThresholds -> FontMetrics -> [Anomaly]
detectAnomalies thresholds =
  map toAnomaly . filter (isAnomalous thresholds) . fmGlyphMetrics
  where
    toAnomaly metrics =
      Anomaly
        { anomalyGlyph = gmGlyph metrics
        , anomalyReasons = classifyAnomaly thresholds metrics
        }

classifyAnomaly :: DetectionThresholds -> GlyphMetrics -> [AnomalyReason]
classifyAnomaly _thresholds _metrics = []

isAnomalous :: DetectionThresholds -> GlyphMetrics -> Bool
isAnomalous thresholds metrics = not (null (classifyAnomaly thresholds metrics))
