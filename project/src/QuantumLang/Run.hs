module QuantumLang.Run
  ( RunConfig (..),
    defaultConfig,
    runSource,
  )
where

import QuantumLang.Interpreter (runProgram)
import QuantumLang.Parser (ParseError (..), parseProgram)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.Random (mkStdGen, setStdGen)

data RunConfig = RunConfig
  { rcLabel :: String,
    rcShowAst :: Bool,
    rcSeed :: Maybe Int
  }

defaultConfig :: String -> RunConfig
defaultConfig label =
  RunConfig
    { rcLabel = label,
      rcShowAst = False,
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
      printParseError err
      exitFailure
    Right prog -> do
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

printParseError :: ParseError -> IO ()
printParseError (ParseError line col msg) =
  putStrLn $ "  line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg

when :: Bool -> IO () -> IO ()
when True io = io
when False _ = pure ()
