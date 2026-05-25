module Analyze.Metrics
  ( analyzeFont,
    buildGlyphMetrics,
    calcReadability,
    calcProportion,
    calcFillDensity,
    calcDistinctness,
    glyphDistance,
  )
where

import Data.Char (digitToInt, isHexDigit)
import Data.List (delete, foldl', maximumBy, sort)
import Data.Ord (comparing)
import Data.Text qualified as Text
import Domain.Types

analyzeFont :: BDFFont -> FontMetrics
analyzeFont font = FontMetrics (map (buildGlyphMetrics font) (fontGlyphs font))

buildGlyphMetrics :: BDFFont -> Glyph -> GlyphMetrics
buildGlyphMetrics font glyph =
  GlyphMetrics
    { gmGlyph = glyph,
      gmReadability = calcReadability glyph,
      gmProportion = calcProportion glyph,
      gmDensity = calcFillDensity glyph,
      gmDistinctness = calcDistinctness glyph (fontGlyphs font)
    }

-- идея взята из алгоритма flood fill:
-- https://en.wikipedia.org/wiki/Flood_fill
-- https://rosettacode.org/wiki/Bitmap/Flood_fill#Haskell
calcReadability :: Glyph -> ReadabilityScore
calcReadability glyph
  | totalPixels == 0 = 0
  | filledPixels == 0 = 0
  | otherwise =
      -- веса эвристические: плотность важнее всего, связность чуть слабее,
      -- покрытие bounding box используется как дополнительный фактор; сумма равна 1
      clamp01
        ( 0.45 * densityBalance
            + 0.35 * mainComponentRatio
            + 0.20 * boundingBoxCoverage
        )
  where
    pixels = glyphPixels glyph
    totalPixels = bitmapArea pixels
    filledPixels = countFilled pixels
    density = ratio filledPixels totalPixels
    densityBalance = 1 - min 1 (abs (density - targetDensity) / targetDensity)
    targetDensity = 0.35
    components = connectedComponents pixels
    largestComponent =
      case components of
        [] -> 0
        _ -> length (maximumBy (comparing length) components)
    mainComponentRatio = ratio largestComponent filledPixels
    boundingBoxCoverage = ratio (filledBoundingBoxArea pixels) totalPixels

-- расчёт основан на размерах bitmap-глифа:
-- https://freetype.org/freetype2/docs/glyphs/glyphs-3.html
calcProportion :: Glyph -> ProportionScore
calcProportion glyph =
  case bitmapSize (glyphPixels glyph) of
    (_width, 0) -> 0
    (width, height) -> fromIntegral width / fromIntegral height

-- строки bitmap в bdf представлены шестнадцатеричными значениями:
-- https://font.tomchen.org/bdf_spec/examples/
calcFillDensity :: Glyph -> FillRatio
calcFillDensity glyph =
  let pixels = glyphPixels glyph
   in ratio (countFilled pixels) (bitmapArea pixels)

-- сравнение основано на модифицированной мере хэмминга для bitmap-глифов:
-- Modified Hamming Distance Measure.pdf
calcDistinctness :: Glyph -> [Glyph] -> DistinctnessScore
calcDistinctness glyph glyphs =
  case filter (/= glyph) glyphs of
    [] -> 1
    others ->
      let nearestDistances = take 5 (filter (> 0) (sort (map (glyphDistance glyph) others)))
      in case nearestDistances of
          [] -> 0
          distances -> clamp01 (sum distances / fromIntegral (length distances))

-- сравнение основано на модифицированной мере хэмминга для bitmap-глифов:
-- Modified Hamming Distance Measure.pdf
glyphDistance :: Glyph -> Glyph -> Double
glyphDistance left right =
  ratio (length (filter not (zipWith (==) leftBits rightBits))) paddedLength
  where
    leftBits = flattenNormalized left width height
    rightBits = flattenNormalized right width height
    paddedLength = max (length leftBits) (length rightBits)
    (leftWidth, leftHeight) = bitmapSize (glyphPixels left)
    (rightWidth, rightHeight) = bitmapSize (glyphPixels right)
    width = max leftWidth rightWidth
    height = max leftHeight rightHeight

glyphPixels :: Glyph -> [[Bool]]
glyphPixels =
  normalizeRows . map decodeHexRow . glyphRows

decodeHexRow :: Text.Text -> [Bool]
decodeHexRow =
  concatMap hexDigitBits . filter isHexDigit . Text.unpack

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

normalizeRows :: [[Bool]] -> [[Bool]]
normalizeRows rows =
  map (padRight width) rows
  where
    width = maximum (0 : map length rows)

padRight :: Int -> [Bool] -> [Bool]
padRight width row = take width (row <> repeat False)

bitmapSize :: [[Bool]] -> (Int, Int)
bitmapSize pixels = (maximum (0 : map length pixels), length pixels)

bitmapArea :: [[Bool]] -> Int
bitmapArea pixels =
  let (width, height) = bitmapSize pixels
   in width * height

countFilled :: [[Bool]] -> Int
countFilled = length . filter id . concat

filledBoundingBoxArea :: [[Bool]] -> Int
filledBoundingBoxArea pixels =
  case filledCoordinates pixels of
    [] -> 0
    coordinates ->
      let xs = map fst coordinates
          ys = map snd coordinates
          width = maximum xs - minimum xs + 1
          height = maximum ys - minimum ys + 1
       in width * height

filledCoordinates :: [[Bool]] -> [(Int, Int)]
filledCoordinates pixels =
  [ (x, y)
    | (y, row) <- zip [0 ..] pixels,
      (x, True) <- zip [0 ..] row
  ]

connectedComponents :: [[Bool]] -> [[(Int, Int)]]
connectedComponents pixels = go [] (filledCoordinates pixels)
  where
    go components [] = components
    go components (point : remaining) =
      let component = floodFill remaining [point] []
          unvisited = foldl' (flip delete) remaining component
       in go (component : components) unvisited

floodFill :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
floodFill _ [] visited = visited
floodFill unvisited (point : queue) visited
  | point `elem` visited = floodFill unvisited queue visited
  | otherwise =
      let adjacent = filter (`elem` unvisited) (neighbors point)
       in floodFill unvisited (queue <> adjacent) (point : visited)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) =
  [ (x + dx, y + dy)
    | dx <- [-1 .. 1],
      dy <- [-1 .. 1],
      (dx, dy) /= (0, 0)
  ]

flattenNormalized :: Glyph -> Int -> Int -> [Bool]
flattenNormalized glyph width height =
  concat (take height (map (padRight width) pixels <> repeat (replicate width False)))
  where
    pixels = glyphPixels glyph

ratio :: Int -> Int -> Double
ratio _ 0 = 0
ratio numerator denominator = fromIntegral numerator / fromIntegral denominator

clamp01 :: Double -> Double
clamp01 = max 0 . min 1
