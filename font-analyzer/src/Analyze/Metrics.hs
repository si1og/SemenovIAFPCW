module Analyze.Metrics
  ( analyzeFont
  , buildGlyphMetrics
  , calcReadability
  , calcProportion
  , calcFillDensity
  , calcDistinctness
  ) where

import Domain.Types

analyzeFont :: BDFFont -> FontMetrics
analyzeFont font = FontMetrics (map (buildGlyphMetrics font) (fontGlyphs font))

buildGlyphMetrics :: BDFFont -> Glyph -> GlyphMetrics
buildGlyphMetrics font glyph =
  GlyphMetrics
    { gmGlyph = glyph
    , gmReadability = calcReadability glyph
    , gmProportion = calcProportion glyph
    , gmDensity = calcFillDensity glyph
    , gmDistinctness = calcDistinctness glyph (fontGlyphs font)
    }

calcReadability :: Glyph -> ReadabilityScore
calcReadability _glyph = 0

calcProportion :: Glyph -> ProportionScore
calcProportion _glyph = 0

calcFillDensity :: Glyph -> FillRatio
calcFillDensity _glyph = 0

calcDistinctness :: Glyph -> [Glyph] -> DistinctnessScore
calcDistinctness _glyph _others = 0
