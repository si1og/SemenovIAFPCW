module Input.BDF
  ( readBDFFile
  , parseBDFContent
  , loadBDFFont
  ) where

import qualified Data.Text as Text
import Domain.Types

readBDFFile :: FilePath -> IO (Either AppError String)
readBDFFile path = Right <$> readFile path

parseBDFContent :: String -> Either ParseError BDFFont
parseBDFContent content
  | null content = Left EmptyBDF
  | otherwise = Right (BDFFont {fontName = Text.pack "unknown", fontGlyphs = []})

loadBDFFont :: FilePath -> IO (Either AppError BDFFont)
loadBDFFont path = do
  contentResult <- readBDFFile path
  pure $ case contentResult of
    Left err -> Left err
    Right content -> either (Left . BDFParseError) Right (parseBDFContent content)
