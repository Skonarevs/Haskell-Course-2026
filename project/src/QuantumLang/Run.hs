module QuantumLang.Run
  ( RunConfig (..),
    defaultConfig,
    runSource,
  )
where

import QuantumLang.Circuit (renderCircuit)
import QuantumLang.Errors (formatParseError)
import QuantumLang.Interpreter (runProgram)
import QuantumLang.Parser (parseProgram)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.Random (mkStdGen, setStdGen)

data RunConfig = RunConfig
  { rcLabel :: String,
    rcShowAst :: Bool,
    rcShowCircuit :: Bool,
    rcSeed :: Maybe Int
  }

defaultConfig :: String -> RunConfig
defaultConfig label =
  RunConfig
    { rcLabel = label,
      rcShowAst = False,
      rcShowCircuit = False,
      rcSeed = Nothing
    }

runSource :: RunConfig -> String -> IO ExitCode
runSource config source = do
  case rcSeed config of
    Nothing -> pure ()
    Just s -> setStdGen (mkStdGen s)
  case parseProgram source of
    Left err -> do
      putStrLn $ "Parse error in " ++ rcLabel config ++ ":"
      putStrLn (formatParseError source err)
      exitFailure
    Right prog -> do
      when (rcShowCircuit config) $ do
        putStrLn (renderCircuit prog)
        putStrLn ""
      when (rcShowAst config) $ do
        putStrLn "Parsed program:"
        print prog
        putStrLn ""
      result <- runProgram prog
      case result of
        Left err -> do
          putStrLn $ "Runtime error: " ++ err
          exitFailure
        Right () -> exitSuccess

when :: Bool -> IO () -> IO ()
when True io = io
when False _ = pure ()
