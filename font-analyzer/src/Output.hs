module Output
  ( printReport
  , printReportWithOptions
  , printError
  , writeReportToFile
  , writeReportToFileWithOptions
  ) where

import Control.Exception (IOException, catch)
import qualified Data.Text.IO as Text
import Domain.Types
import Report (formatReportText, formatReportTextWithOptions)

printReport :: AnalysisReport -> IO ()
printReport = Text.putStrLn . formatReportText

printReportWithOptions :: AnalysisOptions -> AnalysisReport -> IO ()
printReportWithOptions options = Text.putStrLn . formatReportTextWithOptions options

printError :: AppError -> IO ()
printError = print

writeReportToFile :: FilePath -> AnalysisReport -> IO (Either AppError ())
writeReportToFile path = writeReportToFileWithOptions path defaultAnalysisOptions

writeReportToFileWithOptions :: FilePath -> AnalysisOptions -> AnalysisReport -> IO (Either AppError ())
writeReportToFileWithOptions path options report =
  (Right <$> Text.writeFile path (formatReportTextWithOptions options report)) `catch` handleWriteError
  where
    handleWriteError :: IOException -> IO (Either AppError ())
    handleWriteError _ = pure (Left (ReportWriteError path))
