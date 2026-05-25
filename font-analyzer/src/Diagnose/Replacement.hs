module Diagnose.Replacement
  ( findReplacement
  , findReplacements
  , glyphSimilarity
  , normalizeGlyph
  ) where

import Domain.Types
import Analyze.Metrics (glyphDistance)
import Data.List (maximumBy)
import Data.Ord (comparing)

findReplacement :: ReferenceDB -> Anomaly -> Maybe ReplacementSuggestion
findReplacement db anomaly =
  case candidateGlyphs of
    [] -> Nothing
    candidates ->
      let replacement = maximumBy (comparing score) candidates
      in Just
          ReplacementSuggestion
            { rsAnomaly = anomaly
            , rsReplacement = replacement
            , rsSimilarity = score replacement
            }
  where
    sourceGlyph = anomalyGlyph anomaly
    sameCode = filter ((== glyphCode sourceGlyph) . glyphCode . rgGlyph) (referenceGlyphs db)
    candidateGlyphs =
      case sameCode of
        [] -> referenceGlyphs db
        _ -> sameCode
    score = glyphSimilarity sourceGlyph . rgGlyph

findReplacements :: ReferenceDB -> [Anomaly] -> [ReplacementSuggestion]
findReplacements db = foldMap (maybe [] pure . findReplacement db)

glyphSimilarity :: Glyph -> Glyph -> SimilarityScore
glyphSimilarity left right = 1 - glyphDistance left right

normalizeGlyph :: Glyph -> Glyph
normalizeGlyph = id
