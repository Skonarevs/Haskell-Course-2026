module QuantumLang.Interpreter
  ( runProgram,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import QuantumLang.Simulator
import QuantumLang.Types

data Machine = Machine
  { mNumQubits :: Int,
    mAmplitudes :: Amplitudes,
    mClassical :: Map String Int
  }

runProgram :: Program -> IO (Either String ())
runProgram (Program stmts) = do
  result <- execStmts stmts
  pure (void result)
  where
    void (Left err) = Left err
    void (Right _) = Right ()

execStmts :: [Statement] -> IO (Either String Machine)
execStmts [] = pure (Left "program must start with init")
execStmts (Init n : rest) =
  case initMachine n of
    Left err -> pure (Left err)
    Right machine -> execStmts' machine rest
execStmts _ = pure (Left "program must start with init")

execStmts' :: Machine -> [Statement] -> IO (Either String Machine)
execStmts' machine [] = pure (Right machine)
execStmts' machine (stmt : rest) = do
  em' <- execStmt machine stmt
  case em' of
    Left err -> pure (Left err)
    Right machine' -> execStmts' machine' rest

execStmt :: Machine -> Statement -> IO (Either String Machine)
execStmt _ (Init _) = pure (Left "init can only appear once at the start of a program")
execStmt machine (Apply gate qubits) =
  pure $
    case applyGate (mNumQubits machine) gate qubits (mAmplitudes machine) of
      Left err -> Left err
      Right amps -> Right machine {mAmplitudes = amps}
execStmt machine (Measure qubit var)
  | qubit < 0 || qubit >= mNumQubits machine =
      pure (Left "qubit index out of range")
  | otherwise = do
      (bit, amps) <- measureQubit (mNumQubits machine) qubit (mAmplitudes machine)
      pure $
        Right
          machine
            { mAmplitudes = amps,
              mClassical = Map.insert var bit (mClassical machine)
            }
execStmt machine (If var body) =
  case Map.lookup var (mClassical machine) of
    Nothing -> pure (Left ("unknown classical bit: " ++ var))
    Just 1 -> execStmts' machine body
    Just _ -> pure (Right machine)
execStmt machine (Repeat n body)
  | n < 0 = pure (Left "repeat count must be non-negative")
  | otherwise = go n machine
  where
    go 0 m = pure (Right m)
    go k m = do
      em' <- execStmts' m body
      case em' of
        Left err -> pure (Left err)
        Right m' -> go (k - 1) m'
execStmt machine (Print var) =
  case Map.lookup var (mClassical machine) of
    Nothing -> pure (Left ("unknown classical bit: " ++ var))
    Just bit -> do
      putStrLn $ var ++ " = " ++ show bit
      pure (Right machine)

initMachine :: Int -> Either String Machine
initMachine n
  | n < 1 = Left "init requires at least one qubit"
  | otherwise =
      Right
        Machine
          { mNumQubits = n,
            mAmplitudes = initialState n,
            mClassical = Map.empty
          }
