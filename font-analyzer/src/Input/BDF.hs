module Input.BDF
  ( readBDFFile,
    parseBDFContent,
    loadBDFFont,
  )
where

import Control.Exception (IOException, catch)
import Data.Char (isHexDigit)
import Data.List (isPrefixOf)
import Data.Text qualified as Text
import Domain.Types

readBDFFile :: FilePath -> IO (Either AppError String)
readBDFFile path =
  (Right <$> readFile path) `catch` handleReadError
  where
    handleReadError :: IOException -> IO (Either AppError String)
    handleReadError _ = pure (Left (FileReadError path))

-- разбор основан на структуре bdf из спецификации:
-- BDF Specification.pdf
parseBDFContent :: String -> Either ParseError BDFFont
parseBDFContent content
  | null content = Left EmptyBDF
  | not (any (isPrefixOf "STARTFONT") rows) = Left (InvalidBDF (Text.pack "missing STARTFONT"))
  | otherwise = do
      name <- maybe (Left MissingFontName) Right (parseFontName rows)
      glyphs <- traverse parseGlyphBlock (glyphBlocks rows)
      Right (BDFFont {fontName = Text.pack name, fontGlyphs = glyphs})
  where
    rows = lines content

loadBDFFont :: FilePath -> IO (Either AppError BDFFont)
loadBDFFont path = do
  contentResult <- readBDFFile path
  pure $ case contentResult of
    Left err -> Left err
    Right content -> either (Left . BDFParseError) Right (parseBDFContent content)

parseFontName :: [String] -> Maybe String
parseFontName =
  fmap (unwords . drop 1 . words) . findLineWith "FONT "

glyphBlocks :: [String] -> [[String]]
glyphBlocks [] = []
glyphBlocks (row : rows)
  | "STARTCHAR " `isPrefixOf` row =
      let (blockBody, rest) = break (== "ENDCHAR") rows
      in (row : blockBody) : glyphBlocks (drop 1 rest)
  | otherwise = glyphBlocks rows

parseGlyphBlock :: [String] -> Either ParseError Glyph
parseGlyphBlock block = do
  name <- maybe (Left (InvalidBDF (Text.pack "missing STARTCHAR"))) Right (parseGlyphName block)
  code <- maybe (Left (MissingGlyphEncoding (Text.pack name))) Right (parseGlyphEncoding block)
  bitmap <- parseGlyphBitmap name block
  Right
    Glyph
      { glyphName = Text.pack name
      , glyphCode = code
      , glyphRows = map Text.pack bitmap
      }

parseGlyphName :: [String] -> Maybe String
parseGlyphName =
  fmap (unwords . drop 1 . words) . findLineWith "STARTCHAR "

parseGlyphEncoding :: [String] -> Maybe Int
parseGlyphEncoding block =
  case findLineWith "ENCODING " block of
    Just row ->
      case words row of
        [_keyword, value] -> readInt value
        _ -> Nothing
    Nothing -> Nothing

parseGlyphBitmap :: String -> [String] -> Either ParseError [String]
parseGlyphBitmap name block =
  case dropWhile (/= "BITMAP") block of
    [] -> Left (MissingGlyphBitmap (Text.pack name))
    (_bitmapMarker : bitmapRows) ->
      let rows = takeWhile (/= "ENDCHAR") bitmapRows
      in if null rows || any (not . isBitmapRow) rows
          then Left (MissingGlyphBitmap (Text.pack name))
          else Right rows

isBitmapRow :: String -> Bool
isBitmapRow row = not (null row) && all isHexDigit row

findLineWith :: String -> [String] -> Maybe String
findLineWith prefix = safeHead . filter (isPrefixOf prefix)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

readInt :: String -> Maybe Int
readInt value =
  case reads value of
    [(number, "")] -> Just number
    _ -> Nothing
