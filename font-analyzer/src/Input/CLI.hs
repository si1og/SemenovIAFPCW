module Input.CLI
  ( runCLI
  , askBDFPath
  , askSavePath
  , runAnalysisFlow
  ) where

import Analyze.Metrics (analyzeFont)
import Config.App
  ( AppConfig(..)
  )
import Data.Text qualified as Text
import Diagnose.Anomaly (detectAnomalies)
import Diagnose.Replacement (findReplacements)
import Input.BDF (loadBDFFont)
import Logging qualified
import Output qualified
import Reference.Database (buildReferenceDB)
import Report (assembleReport)

runCLI :: AppConfig -> IO ()
runCLI config = do
  putStrLn "путь к анализируемому bdf-файлу:"
  inputPath <- askBDFPath
  putStrLn "путь для сохранения отчёта (пустая строка — путь по умолчанию):"
  outputPath <- askSavePath
  runAnalysisFlow config inputPath outputPath

askBDFPath :: IO FilePath
askBDFPath = getLine

askSavePath :: IO (Maybe FilePath)
askSavePath = do
  path <- getLine
  pure $ if null path then Nothing else Just path

runAnalysisFlow :: AppConfig -> FilePath -> Maybe FilePath -> IO ()
runAnalysisFlow config inputPath outputPath = do
  Logging.logEvent (acLogConfig config) Logging.Info (Text.pack "запущен анализ bdf-файла")
  inputFontResult <- loadBDFFont inputPath
  referenceFontResult <- loadBDFFont (acReferenceFontPath config)
  case (inputFontResult, referenceFontResult) of
    (Left err, _) -> Output.printError err
    (_, Left err) -> Output.printError err
    (Right inputFont, Right referenceFont) -> do
      let metrics = analyzeFont inputFont
          anomalies = detectAnomalies (acThresholds config) metrics
          referenceDB = buildReferenceDB referenceFont
          replacements = findReplacements referenceDB anomalies
          report = assembleReport inputFont metrics anomalies replacements
          reportPath = maybe (acReportOutputPath config) id outputPath
      Output.printReport report
      writeResult <- Output.writeReportToFile reportPath report
      case writeResult of
        Left err -> Output.printError err
        Right () ->
          Logging.logEvent (acLogConfig config) Logging.Info (Text.pack "отчёт сохранён")
