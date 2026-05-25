module Main (main) where

import Analyze.Metrics
import Config.App
import Data.Text qualified as Text
import Diagnose.Anomaly
import Domain.Types
import Test.QuickCheck

main :: IO ()
main = runTests

runTests :: IO ()
runTests = do
  putStrLn "Тестирование метрики плотности заполнения глифа.."
  quickCheck prop_densityInRange
  putStrLn "Тестирование метрики читаемости глифа.."
  quickCheck prop_readabilityInRange
  putStrLn "Тестирование числа метрик глифа.."
  quickCheck prop_analyzePreservesGlyphCount
  putStrLn "Тестирование низкой плотности заполнения глифа.."
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

prop_detectNoAnomaliesForNormalMetrics :: Glyph -> Bool
prop_detectNoAnomaliesForNormalMetrics glyph =
  null (detectAnomalies defaultThresholds metrics)
  where
    metrics =
      FontMetrics
        [ GlyphMetrics
            { gmGlyph = glyph,
              gmReadability = 0.7,
              gmProportion = 1,
              gmDensity = 0.4,
              gmDistinctness = 0.7,
              gmDistinctnessGlyph = Nothing
            }
        ]

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
      <*> arbitrary

arbitraryText :: Gen Text.Text
arbitraryText = Text.pack <$> arbitrary
