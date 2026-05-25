module Output
  ( printReport
  , printReportWithOptions
  , printError
  , writeReportToFile
  , writeReportToFileWithOptions
  ) where

import Control.Exception (IOException, try)
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
writeReportToFileWithOptions path options report = do
  result <- try (Text.writeFile path (formatReportTextWithOptions options report)) :: IO (Either IOException ())
  pure $ case result of
    Left _ -> Left (ReportWriteError path)
    Right () -> Right ()
