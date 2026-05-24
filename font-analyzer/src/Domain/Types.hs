module Domain.Types
  ( BDFFont(..)
  , Glyph(..)
  , FontMetrics(..)
  , GlyphMetrics(..)
  , ReadabilityScore
  , ProportionScore
  , FillRatio
  , DistinctnessScore
  , SimilarityScore
  , Anomaly(..)
  , AnomalyReason(..)
  , ReferenceDB(..)
  , ReferenceGlyph(..)
  , ReplacementSuggestion(..)
  , AnalysisReport(..)
  , AppError(..)
  , ParseError(..)
  ) where

import Data.Text (Text)

data BDFFont = BDFFont
  { fontName :: Text
  , fontGlyphs :: [Glyph]
  }
  deriving (Eq, Show)

data Glyph = Glyph
  { glyphName :: Text
  , glyphCode :: Int
  , glyphRows :: [Text]
  }
  deriving (Eq, Show)

data FontMetrics = FontMetrics
  { fmGlyphMetrics :: [GlyphMetrics]
  }
  deriving (Eq, Show)

data GlyphMetrics = GlyphMetrics
  { gmGlyph :: Glyph
  , gmReadability :: ReadabilityScore
  , gmProportion :: ProportionScore
  , gmDensity :: FillRatio
  , gmDistinctness :: DistinctnessScore
  }
  deriving (Eq, Show)

type ReadabilityScore = Double
type ProportionScore = Double
type FillRatio = Double
type DistinctnessScore = Double
type SimilarityScore = Double

data Anomaly = Anomaly
  { anomalyGlyph :: Glyph
  , anomalyReasons :: [AnomalyReason]
  }
  deriving (Eq, Show)

data AnomalyReason
  = LowReadability
  | BadProportion
  | TooSparse
  | TooDense
  | LowDistinctness
  deriving (Eq, Show)

data ReferenceDB = ReferenceDB
  { referenceGlyphs :: [ReferenceGlyph]
  }
  deriving (Eq, Show)

data ReferenceGlyph = ReferenceGlyph
  { rgGlyph :: Glyph
  }
  deriving (Eq, Show)

data ReplacementSuggestion = ReplacementSuggestion
  { rsAnomaly :: Anomaly
  , rsReplacement :: ReferenceGlyph
  , rsSimilarity :: SimilarityScore
  }
  deriving (Eq, Show)

data AnalysisReport = AnalysisReport
  { reportFont :: BDFFont
  , reportMetrics :: FontMetrics
  , reportAnomalies :: [Anomaly]
  , reportReplacements :: [ReplacementSuggestion]
  }
  deriving (Eq, Show)

data AppError
  = FileReadError FilePath
  | BDFParseError ParseError
  | ReportWriteError FilePath
  | InvalidInput Text
  deriving (Eq, Show)

data ParseError
  = EmptyBDF
  | InvalidBDF Text
  deriving (Eq, Show)
