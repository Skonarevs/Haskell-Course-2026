module Main where

import QuantumLang.Run (RunConfig (..), defaultConfig, runSource)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn helpText >> exitSuccess
    ["-h"] -> putStrLn helpText >> exitSuccess
    _ ->
      case parseArgs args of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right (config, path) -> do
          source <- readFile path
          putStrLn "quantum-lang (part 4 - complete)"
          putStrLn ""
          exitWith =<< runSource config {rcLabel = path} source

parseArgs :: [String] -> Either String (RunConfig, FilePath)
parseArgs args = go (defaultConfig "program.ql") args
  where
    go _ [] = Left "no input file specified (try --help)"
    go cfg [path]
      | isOption path = Left $ "unknown option: " ++ path
      | otherwise = Right (cfg, path)
    go cfg ("--ast" : rest) = go cfg {rcShowAst = True} rest
    go cfg ("--seed" : value : rest) =
      case reads value of
        [(seed, "")] -> go cfg {rcSeed = Just seed} rest
        _ -> Left "invalid --seed value (expected integer)"
    go _ (opt : _) = Left $ "unknown option: " ++ opt

    isOption ('-' : '-' : _) = True
    isOption _ = False

helpText :: String
helpText =
  unlines
    [ "quantum-lang - quantum circuit DSL interpreter",
      "",
      "Usage:",
      "  quantum-lang [--ast] [--seed N] <file.ql>",
      "",
      "Options:",
      "  --ast       parse and print the AST before running",
      "  --seed N    fix the RNG seed for reproducible measurement outcomes",
      "  --help      show this help text",
      "",
      "Examples:",
      "  quantum-lang examples/bell.ql",
      "  quantum-lang examples/teleport.ql",
      "  quantum-lang examples/deutsch.ql",
      "  quantum-lang --seed 42 examples/bell.ql",
      "",
      "Example programs:",
      "  examples/bell.ql             Bell-state correlations",
      "  examples/teleport.ql         quantum teleportation of |1>",
      "  examples/deutsch.ql          Deutsch algorithm (balanced oracle)",
      "  examples/deutsch-constant.ql Deutsch algorithm (constant oracle)"
    ]
