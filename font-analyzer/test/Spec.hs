module Main (main) where

import Analyze.Metrics
import Config.App
import Diagnose.Anomaly
import qualified Data.Text as Text
import Domain.Types
import Test.QuickCheck

main :: IO ()
main = runTests

runTests :: IO ()
runTests = do
  quickCheck prop_densityInRange
  quickCheck prop_readabilityInRange
  quickCheck prop_analyzePreservesGlyphCount
  quickCheck prop_detectNoAnomaliesForNormalMetrics

prop_densityInRange :: Glyph -> Bool
prop_densityInRange glyph =
  let density = calcFillDensity glyph
  in density >= 0 && density <= 1

prop_readabilityInRange :: Glyph -> Bool
prop_readabilityInRange glyph =
  let readability = calcReadability glyph
  in readability >= 0 && readability <= 1

prop_analyzePreservesGlyphCount :: BDFFont -> Bool
prop_analyzePreservesGlyphCount font =
  length (fmGlyphMetrics (analyzeFont font)) == length (fontGlyphs font)

prop_detectNoAnomaliesForNormalMetrics :: FontMetrics -> Bool
prop_detectNoAnomaliesForNormalMetrics metrics =
  null (detectAnomalies defaultThresholds metrics)

instance Arbitrary Glyph where
  arbitrary = Glyph <$> arbitraryText <*> arbitrary <*> listOf arbitraryText

instance Arbitrary BDFFont where
  arbitrary = BDFFont <$> arbitraryText <*> arbitrary

instance Arbitrary FontMetrics where
  arbitrary = FontMetrics <$> arbitrary

instance Arbitrary GlyphMetrics where
  arbitrary =
    GlyphMetrics
      <$> arbitrary
      <*> choose (0, 1)
      <*> choose (0, 10)
      <*> choose (0, 1)
      <*> choose (0, 1)

arbitraryText :: Gen Text.Text
arbitraryText = Text.pack <$> arbitrary
