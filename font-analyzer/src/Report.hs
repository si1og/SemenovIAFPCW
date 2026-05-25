module Report
  ( assembleReport,
    formatReportText,
    formatReportTextWithOptions,
  )
where

import Data.Char (digitToInt, isHexDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types
import Numeric (showFFloat)

assembleReport ::
  BDFFont ->
  FontMetrics ->
  DetectionThresholds ->
  [Anomaly] ->
  [ReplacementSuggestion] ->
  AnalysisReport
assembleReport font metrics thresholds anomalies replacements =
  AnalysisReport
    { reportFont = font,
      reportMetrics = metrics,
      reportThresholds = thresholds,
      reportAnomalies = anomalies,
      reportReplacements = replacements
    }

formatReportText :: AnalysisReport -> Text
formatReportText = formatReportTextWithOptions defaultAnalysisOptions

formatReportTextWithOptions :: AnalysisOptions -> AnalysisReport -> Text
formatReportTextWithOptions options report =
  Text.intercalate
    (Text.pack "\n\n")
    [ formatHeader report,
      formatPreamble options (reportThresholds report),
      formatMetrics options (reportThresholds report) (reportMetrics report),
      formatAnomalies report,
      formatReplacements (reportReplacements report)
    ]

formatHeader :: AnalysisReport -> Text
formatHeader report =
  Text.unlines
    [ Text.pack "отчёт анализа bdf-шрифта",
      Text.pack "шрифт: " <> fontName (reportFont report),
      Text.pack "глифов: " <> showText (length (fontGlyphs (reportFont report))),
      Text.pack "аномалий: " <> showText (length (reportAnomalies report)),
      Text.pack "предложений замены: " <> showText (length (reportReplacements report))
    ]

formatPreamble :: AnalysisOptions -> DetectionThresholds -> Text
formatPreamble options thresholds =
  Text.unlines (Text.pack "параметры анализа:" : selectedMetricDescriptions)
  where
    selectedMetricDescriptions =
      concat
        [ [ Text.pack "readability  — читаемость глифа, допустимо: "
              <> showScore (dtMinReadability thresholds)
              <> Text.pack " <= value <= 1.0"
          | aoAnalyzeReadability options
          ]
        , [ Text.pack "proportion   — отношение ширины bitmap к высоте"
          | aoAnalyzeProportion options
          ]
        , [ Text.pack "density      — доля закрашенных пикселей, допустимо: "
              <> showScore (dtMinDensity thresholds)
              <> Text.pack " <= value <= "
              <> showScore (dtMaxDensity thresholds)
          | aoAnalyzeDensity options
          ]
        , [ Text.pack "distinctness — различимость относительно ближайших отличающихся глифов, допустимо: "
              <> showScore (dtMinDistinctness thresholds)
              <> Text.pack " <= value <= 1.0"
          | aoAnalyzeDistinctness options
          ]
        ]

formatMetrics :: AnalysisOptions -> DetectionThresholds -> FontMetrics -> Text
formatMetrics options thresholds metrics =
  Text.intercalate (Text.pack "\n") (Text.pack "метрики глифов:" : header : separator : rows)
  where
    glyphMetrics = fmGlyphMetrics metrics
    metricRows = map (metricColumns options thresholds) glyphMetrics
    widths = columnWidths ([metricHeader options] <> metricRows)
    header = formatColumns widths (metricHeader options)
    separator = Text.pack (replicate (Text.length header) '-')
    rows = zipWith (formatGlyphMetricBlock options thresholds widths) glyphMetrics metricRows

formatGlyphMetricBlock :: AnalysisOptions -> DetectionThresholds -> [Int] -> GlyphMetrics -> [Text] -> Text
formatGlyphMetricBlock options thresholds widths metrics columns =
  Text.unlines
    ( [ formatColumns widths columns,
        Text.pack "bitmap:",
        formatGlyphBitmap (gmGlyph metrics)
      ]
        <> comparedGlyphBlock options thresholds metrics
    )

comparedGlyphBlock :: AnalysisOptions -> DetectionThresholds -> GlyphMetrics -> [Text]
comparedGlyphBlock options thresholds metrics =
  case gmDistinctnessGlyph metrics of
    Just glyph
      | aoAnalyzeDistinctness options && gmDistinctness metrics < dtMinDistinctness thresholds ->
          [ Text.pack "визуально похожий глиф: " <> formatGlyphRef glyph,
            formatGlyphBitmap glyph
          ]
    _ -> []

metricHeader :: AnalysisOptions -> [Text]
metricHeader options =
  [Text.pack "glyph", Text.pack "code"]
    <> [Text.pack "readability" | aoAnalyzeReadability options]
    <> [Text.pack "proportion" | aoAnalyzeProportion options]
    <> [Text.pack "density" | aoAnalyzeDensity options]
    <> [Text.pack "distinctness" | aoAnalyzeDistinctness options]
    <> [Text.pack "compared_with" | aoAnalyzeDistinctness options]

metricColumns :: AnalysisOptions -> DetectionThresholds -> GlyphMetrics -> [Text]
metricColumns options thresholds metrics =
  [ glyphName (gmGlyph metrics),
    showText (glyphCode (gmGlyph metrics))
  ]
    <> [showScore (gmReadability metrics) | aoAnalyzeReadability options]
    <> [showScore (gmProportion metrics) | aoAnalyzeProportion options]
    <> [showScore (gmDensity metrics) | aoAnalyzeDensity options]
    <> [showScore (gmDistinctness metrics) | aoAnalyzeDistinctness options]
    <> [ comparedWithValue | aoAnalyzeDistinctness options]
  where
    comparedWithValue =
      if gmDistinctness metrics < dtMinDistinctness thresholds
        then maybe (Text.pack "-") formatGlyphRef (gmDistinctnessGlyph metrics)
        else Text.pack "-"

formatAnomalies :: AnalysisReport -> Text
formatAnomalies report
  | null (reportAnomalies report) = Text.pack "аномалии: не найдены"
  | otherwise = Text.unlines (Text.pack "аномалии:" : header : separator : rows)
  where
    anomalyRows = map (anomalyColumns report) (reportAnomalies report)
    widths = columnWidths ([anomalyHeader] <> anomalyRows)
    header = formatColumns widths anomalyHeader
    separator = Text.pack (replicate (Text.length header) '-')
    rows = map (formatColumns widths) anomalyRows

anomalyHeader :: [Text]
anomalyHeader =
  [ Text.pack "glyph",
    Text.pack "code",
    Text.pack "нарушенные критерии"
  ]

anomalyColumns :: AnalysisReport -> Anomaly -> [Text]
anomalyColumns report anomaly =
  [ glyphName (anomalyGlyph anomaly),
    showText (glyphCode (anomalyGlyph anomaly)),
    Text.intercalate (Text.pack "; ") (map (formatAnomalyReason thresholds metrics) (anomalyReasons anomaly))
  ]
  where
    thresholds = reportThresholds report
    metrics = findGlyphMetrics (reportMetrics report) (anomalyGlyph anomaly)

findGlyphMetrics :: FontMetrics -> Glyph -> Maybe GlyphMetrics
findGlyphMetrics metrics glyph =
  safeHead (filter ((== glyph) . gmGlyph) (fmGlyphMetrics metrics))

formatAnomalyReason :: DetectionThresholds -> Maybe GlyphMetrics -> AnomalyReason -> Text
formatAnomalyReason thresholds metrics reason =
  case (metrics, reason) of
    (Just glyphMetrics, LowReadability) ->
      Text.pack "readability: "
        <> showScore (gmReadability glyphMetrics)
        <> Text.pack " < "
        <> showScore (dtMinReadability thresholds)
    (Just glyphMetrics, TooSparse) ->
      Text.pack "density: "
        <> showScore (gmDensity glyphMetrics)
        <> Text.pack " < "
        <> showScore (dtMinDensity thresholds)
    (Just glyphMetrics, TooDense) ->
      Text.pack "density: "
        <> showScore (gmDensity glyphMetrics)
        <> Text.pack " > "
        <> showScore (dtMaxDensity thresholds)
    (Just glyphMetrics, LowDistinctness) ->
      Text.pack "distinctness: "
        <> showScore (gmDistinctness glyphMetrics)
        <> Text.pack " < "
        <> showScore (dtMinDistinctness thresholds)
    (_, BadProportion) -> Text.pack "proportion: нарушена пропорция"
    _ -> formatAnomalyReasonName reason

formatAnomalyReasonName :: AnomalyReason -> Text
formatAnomalyReasonName LowReadability = Text.pack "низкая читаемость"
formatAnomalyReasonName BadProportion = Text.pack "нарушенная пропорция"
formatAnomalyReasonName TooSparse = Text.pack "низкая плотность"
formatAnomalyReasonName TooDense = Text.pack "высокая плотность"
formatAnomalyReasonName LowDistinctness = Text.pack "низкая различимость"

formatReplacements :: [ReplacementSuggestion] -> Text
formatReplacements [] = Text.pack "замены: не предложены"
formatReplacements suggestions =
  Text.intercalate (Text.pack "\n") (Text.pack "замены:" : header : separator : rows)
  where
    replacementRows = map replacementColumns suggestions
    widths = columnWidths ([replacementHeader] <> replacementRows)
    header = formatColumns widths replacementHeader
    separator = Text.pack (replicate (Text.length header) '-')
    rows = zipWith (formatReplacementBlock widths) suggestions replacementRows

replacementHeader :: [Text]
replacementHeader =
  [ Text.pack "source",
    Text.pack "code",
    Text.pack "replacement",
    Text.pack "replacement_code",
    Text.pack "similarity"
  ]

replacementColumns :: ReplacementSuggestion -> [Text]
replacementColumns suggestion =
  [ glyphName source,
    showText (glyphCode source),
    glyphName replacement,
    showText (glyphCode replacement),
    showScore (rsSimilarity suggestion)
  ]
  where
    source = anomalyGlyph (rsAnomaly suggestion)
    replacement = rgGlyph (rsReplacement suggestion)

formatReplacementBlock :: [Int] -> ReplacementSuggestion -> [Text] -> Text
formatReplacementBlock widths _suggestion columns =
  Text.unlines [formatColumns widths columns]

formatGlyphBitmap :: Glyph -> Text
formatGlyphBitmap glyph =
  Text.unlines (map formatBitmapRow (glyphRows glyph))

formatBitmapRow :: Text -> Text
formatBitmapRow =
  Text.pack . map pixelChar . concatMap hexDigitBits . filter isHexDigit . Text.unpack

hexDigitBits :: Char -> [Bool]
hexDigitBits char =
  [ testBitValue 8,
    testBitValue 4,
    testBitValue 2,
    testBitValue 1
  ]
  where
    value = digitToInt char
    testBitValue mask = value `div` mask `mod` 2 == 1

pixelChar :: Bool -> Char
pixelChar True = '#'
pixelChar False = '.'

formatGlyphRef :: Glyph -> Text
formatGlyphRef glyph =
  glyphName glyph <> Text.pack " (" <> showText (glyphCode glyph) <> Text.pack ")"

formatColumns :: [Int] -> [Text] -> Text
formatColumns widths columns =
  Text.intercalate (Text.pack " | ") (zipWith padRightText widths columns)

columnWidths :: [[Text]] -> [Int]
columnWidths rows =
  map maximum columns
  where
    columns = transpose (map (map Text.length) rows)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose rows
  | any null rows = []
  | otherwise = map head rows : transpose (map tail rows)

padRightText :: Int -> Text -> Text
padRightText width value =
  value <> Text.replicate (width - Text.length value) (Text.pack " ")

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

showScore :: Double -> Text
showScore value = Text.pack (showFFloat (Just 3) value "")

showText :: (Show a) => a -> Text
showText = Text.pack . show
