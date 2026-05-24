module Reference.Database
  ( defaultReferenceFontPath
  , buildReferenceDB
  , findReferenceGlyphs
  ) where

import Domain.Types

defaultReferenceFontPath :: FilePath
defaultReferenceFontPath = "data/terminus-8x16.bdf"

buildReferenceDB :: BDFFont -> ReferenceDB
buildReferenceDB font = ReferenceDB (map ReferenceGlyph (fontGlyphs font))

findReferenceGlyphs :: ReferenceDB -> Char -> [ReferenceGlyph]
findReferenceGlyphs db _char = referenceGlyphs db
