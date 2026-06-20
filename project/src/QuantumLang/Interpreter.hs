module QuantumLang.Interpreter
  ( runProgram,
  )
where

import QuantumLang.Types

-- | Stub interpreter for part 2
-- The real state-vector simulator in part 3.
runProgram :: Program -> IO ()
runProgram (Program stmts) = mapM_ execStmt stmts

execStmt :: Statement -> IO ()
execStmt stmt = case stmt of
  Init n ->
    putStrLn $ "[stub] init register with " ++ show n ++ " qubit(s)"
  Apply gate qubits ->
    putStrLn $ "[stub] apply " ++ show gate ++ " to qubits " ++ show qubits
  Measure qubit var ->
    putStrLn $ "[stub] measure qubit " ++ show qubit ++ " -> " ++ var
  If var body -> do
    putStrLn $ "[stub] if " ++ var ++ " == 1 then"
    mapM_ execStmt body
    putStrLn "[stub] end if"
  Repeat n body -> do
    putStrLn $ "[stub] repeat " ++ show n ++ " times"
    mapM_ execStmt body
    putStrLn "[stub] end repeat"
  Print var ->
    putStrLn $ "[stub] print " ++ var
