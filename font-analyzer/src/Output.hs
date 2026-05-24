module Output
  ( printReport
  , printError
  , writeReportToFile
  ) where

import qualified Data.Text.IO as Text
import Domain.Types
import Report (formatReportText)

printReport :: AnalysisReport -> IO ()
printReport = Text.putStrLn . formatReportText

printError :: AppError -> IO ()
printError = print

writeReportToFile :: FilePath -> AnalysisReport -> IO (Either AppError ())
writeReportToFile path report = do
  Text.writeFile path (formatReportText report)
  pure (Right ())
