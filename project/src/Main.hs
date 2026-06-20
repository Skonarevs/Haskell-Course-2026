module Main where

import QuantumLang.Interpreter (runProgram)
import QuantumLang.Parser (ParseError (..), parseProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  (label, source) <-
    case args of
      [] -> do
        src <- readFile "examples/bell.ql"
        pure ("examples/bell.ql", src)
      [path] -> do
        src <- readFile path
        pure (path, src)
      _ -> do
        putStrLn "Usage: quantum-lang [program.ql]"
        exitFailure
  putStrLn "quantum-lang (part 2 - parser skeleton)"
  putStrLn ""
  case parseProgram source of
    Left err -> do
      putStrLn $ "Parse error in " ++ label ++ ":"
      printParseError err
      exitFailure
    Right prog -> do
      putStrLn "Parsed program:"
      print prog
      putStrLn ""
      putStrLn "Running stub interpreter:"
      runProgram prog

printParseError :: ParseError -> IO ()
printParseError (ParseError line col msg) =
  putStrLn $ "  line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg
