module Diagnose.Anomaly
  ( detectAnomalies
  , detectAnomaliesWithOptions
  , classifyAnomaly
  , classifyAnomalyWithOptions
  , isAnomalous
  , isAnomalousWithOptions
  ) where

import Domain.Types

detectAnomalies :: DetectionThresholds -> FontMetrics -> [Anomaly]
detectAnomalies = detectAnomaliesWithOptions defaultAnalysisOptions

detectAnomaliesWithOptions :: AnalysisOptions -> DetectionThresholds -> FontMetrics -> [Anomaly]
detectAnomaliesWithOptions options thresholds =
  map toAnomaly . filter (isAnomalousWithOptions options thresholds) . fmGlyphMetrics
  where
    toAnomaly metrics =
      Anomaly
        { anomalyGlyph = gmGlyph metrics
        , anomalyReasons = classifyAnomalyWithOptions options thresholds metrics
        }

classifyAnomaly :: DetectionThresholds -> GlyphMetrics -> [AnomalyReason]
classifyAnomaly = classifyAnomalyWithOptions defaultAnalysisOptions

classifyAnomalyWithOptions :: AnalysisOptions -> DetectionThresholds -> GlyphMetrics -> [AnomalyReason]
classifyAnomalyWithOptions options thresholds metrics =
  concat
    [ [LowReadability | aoAnalyzeReadability options && gmReadability metrics < dtMinReadability thresholds]
    , [] -- proportion is calculated and shown as a metric; no threshold is defined for it in DetectionThresholds.
    , [TooSparse | aoAnalyzeDensity options && gmDensity metrics < dtMinDensity thresholds]
    , [TooDense | aoAnalyzeDensity options && gmDensity metrics > dtMaxDensity thresholds]
    , [LowDistinctness | aoAnalyzeDistinctness options && gmDistinctness metrics < dtMinDistinctness thresholds]
    ]

isAnomalous :: DetectionThresholds -> GlyphMetrics -> Bool
isAnomalous = isAnomalousWithOptions defaultAnalysisOptions

isAnomalousWithOptions :: AnalysisOptions -> DetectionThresholds -> GlyphMetrics -> Bool
isAnomalousWithOptions options thresholds metrics =
  not (null (classifyAnomalyWithOptions options thresholds metrics))
