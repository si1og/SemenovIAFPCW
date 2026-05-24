module Diagnose.Replacement
  ( findReplacement
  , findReplacements
  , glyphSimilarity
  , normalizeGlyph
  ) where

import Domain.Types

findReplacement :: ReferenceDB -> Anomaly -> Maybe ReplacementSuggestion
findReplacement _db _anomaly = Nothing

findReplacements :: ReferenceDB -> [Anomaly] -> [ReplacementSuggestion]
findReplacements db = foldMap (maybe [] pure . findReplacement db)

glyphSimilarity :: Glyph -> Glyph -> SimilarityScore
glyphSimilarity _left _right = 0

normalizeGlyph :: Glyph -> Glyph
normalizeGlyph = id
