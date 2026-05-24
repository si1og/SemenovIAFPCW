module Diagnose.Replacement
  ( findReplacement
  , findReplacements
  , glyphSimilarity
  , normalizeGlyph
  ) where

import Domain.Types
import Analyze.Metrics (glyphDistance)

findReplacement :: ReferenceDB -> Anomaly -> Maybe ReplacementSuggestion
findReplacement _db _anomaly = Nothing

findReplacements :: ReferenceDB -> [Anomaly] -> [ReplacementSuggestion]
findReplacements db = foldMap (maybe [] pure . findReplacement db)

glyphSimilarity :: Glyph -> Glyph -> SimilarityScore
glyphSimilarity left right = 1 - glyphDistance left right

normalizeGlyph :: Glyph -> Glyph
normalizeGlyph = id
