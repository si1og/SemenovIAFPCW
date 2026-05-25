module Input.CLI
  ( runCLI
  , askBDFPath
  , askSavePath
  , askAnalysisOptions
  , runAnalysisFlow
  , runAnalysisFlowWithOptions
  ) where

import Analyze.Metrics (analyzeFont)
import Config.App
  ( AppConfig(..)
  )
import Data.Char (toLower)
import Data.Text qualified as Text
import Diagnose.Anomaly (detectAnomaliesWithOptions)
import Diagnose.Replacement (findReplacements)
import Domain.Types (AnalysisOptions(..), defaultAnalysisOptions)
import Input.BDF (loadBDFFont)
import Logging qualified
import Output qualified
import Reference.Database (buildReferenceDB)
import Report (assembleReport)

runCLI :: AppConfig -> IO ()
runCLI config = menuLoop defaultAnalysisOptions
  where
    menuLoop options = do
      putStrLn ""
      putStrLn "меню:"
      putStrLn "1 — запустить анализ bdf-файла"
      putStrLn "2 — настроить параметры анализа"
      putStrLn "3 — показать текущие параметры анализа"
      putStrLn "0 — выход"
      putStrLn "выберите действие:"
      action <- getLine
      case action of
        "1" -> do
          Logging.logEvent (acLogConfig config) Logging.Info (Text.pack "пользователь выбрал запуск анализа")
          putStrLn "путь к анализируемому bdf-файлу:"
          inputPath <- askBDFPath
          putStrLn "путь для сохранения отчёта (пустая строка — путь по умолчанию):"
          outputPath <- askSavePath
          runAnalysisFlowWithOptions config options inputPath outputPath
          menuLoop options
        "2" -> do
          Logging.logEvent (acLogConfig config) Logging.Info (Text.pack "пользователь выбрал настройку параметров анализа")
          newOptions <- askAnalysisOptions
          Logging.logEvent (acLogConfig config) Logging.Info (Text.pack ("параметры анализа изменены: " <> show newOptions))
          menuLoop newOptions
        "3" -> do
          putStrLn (formatAnalysisOptions options)
          menuLoop options
        "0" -> do
          Logging.logEvent (acLogConfig config) Logging.Info (Text.pack "пользователь завершил работу")
          putStrLn "выход"
        _ -> do
          Logging.logEvent (acLogConfig config) Logging.Warning (Text.pack ("некорректный пункт меню: " <> action))
          putStrLn "некорректный ввод, выберите пункт меню ещё раз"
          menuLoop options

askBDFPath :: IO FilePath
askBDFPath = getLine

askSavePath :: IO (Maybe FilePath)
askSavePath = do
  path <- getLine
  pure $ if null path then Nothing else Just path

askAnalysisOptions :: IO AnalysisOptions
askAnalysisOptions = do
  putStrLn "настройка параметров анализа: пустой ввод означает 'да'"
  analyzeReadability <- askYesNo "анализировать readability / читаемость?" True
  analyzeProportion <- askYesNo "анализировать proportion / пропорции?" True
  analyzeDensity <- askYesNo "анализировать density / плотность?" True
  analyzeDistinctness <- askYesNo "анализировать distinctness / различимость?" True
  pure
    AnalysisOptions
      { aoAnalyzeReadability = analyzeReadability
      , aoAnalyzeProportion = analyzeProportion
      , aoAnalyzeDensity = analyzeDensity
      , aoAnalyzeDistinctness = analyzeDistinctness
      }

askYesNo :: String -> Bool -> IO Bool
askYesNo question defaultValue = do
  putStrLn (question <> " [y/n]")
  answer <- getLine
  case map toLower answer of
    "" -> pure defaultValue
    "y" -> pure True
    "yes" -> pure True
    "д" -> pure True
    "да" -> pure True
    "n" -> pure False
    "no" -> pure False
    "н" -> pure False
    "нет" -> pure False
    _ -> do
      putStrLn "некорректный ввод, введите y/n или да/нет"
      askYesNo question defaultValue

formatAnalysisOptions :: AnalysisOptions -> String
formatAnalysisOptions options =
  unlines
    [ "текущие параметры анализа:"
    , "readability / читаемость: " <> yesNo (aoAnalyzeReadability options)
    , "proportion / пропорции: " <> yesNo (aoAnalyzeProportion options)
    , "density / плотность: " <> yesNo (aoAnalyzeDensity options)
    , "distinctness / различимость: " <> yesNo (aoAnalyzeDistinctness options)
    ]
  where
    yesNo True = "да"
    yesNo False = "нет"

runAnalysisFlow :: AppConfig -> FilePath -> Maybe FilePath -> IO ()
runAnalysisFlow config = runAnalysisFlowWithOptions config defaultAnalysisOptions

runAnalysisFlowWithOptions :: AppConfig -> AnalysisOptions -> FilePath -> Maybe FilePath -> IO ()
runAnalysisFlowWithOptions config analysisOptions inputPath outputPath = do
  Logging.logEvent (acLogConfig config) Logging.Info (Text.pack "запущен анализ bdf-файла")
  inputFontResult <- loadBDFFont inputPath
  referenceFontResult <- loadBDFFont (acReferenceFontPath config)
  case (inputFontResult, referenceFontResult) of
    (Left err, _) -> do
      Logging.logEvent (acLogConfig config) Logging.Error (Text.pack ("ошибка чтения анализируемого файла: " <> show err))
      Output.printError err
    (_, Left err) -> do
      Logging.logEvent (acLogConfig config) Logging.Error (Text.pack ("ошибка чтения эталонного файла: " <> show err))
      Output.printError err
    (Right inputFont, Right referenceFont) -> do
      let metrics = analyzeFont inputFont
          anomalies = detectAnomaliesWithOptions analysisOptions (acThresholds config) metrics
          referenceDB = buildReferenceDB referenceFont
          replacements = findReplacements referenceDB anomalies
          report = assembleReport inputFont metrics (acThresholds config) anomalies replacements
          reportPath = maybe (acReportOutputPath config) id outputPath
      Output.printReportWithOptions analysisOptions report
      writeResult <- Output.writeReportToFileWithOptions reportPath analysisOptions report
      case writeResult of
        Left err -> do
          Logging.logEvent (acLogConfig config) Logging.Error (Text.pack ("ошибка записи отчёта: " <> show err))
          Output.printError err
        Right () ->
          Logging.logEvent (acLogConfig config) Logging.Info (Text.pack ("отчёт сохранён: " <> reportPath))
