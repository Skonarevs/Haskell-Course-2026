module QuantumLang.Circuit
  ( renderCircuit,
  )
where

import Data.List (intercalate)
import QuantumLang.Types

data CircuitOp
  = ColGate Gate [Int]
  | ColMeasure Int String
  deriving (Eq, Show)

renderCircuit :: Program -> String
renderCircuit (Program stmts) =
  case findInit stmts of
    Nothing -> "(no circuit: missing init)"
    Just n ->
      let ops = collectOps stmts
       in if null ops
            then "(empty circuit)"
            else
              unlines $
                ( ("Circuit (" ++ show n ++ " qubit(s)):")
                    : [renderQubitLine q ops | q <- [0 .. n - 1]]
                )
  where
    findInit [] = Nothing
    findInit (Init k : _) = Just k
    findInit (_ : rest) = findInit rest

collectOps :: [Statement] -> [CircuitOp]
collectOps = concatMap stmtOps

stmtOps :: Statement -> [CircuitOp]
stmtOps stmt = case stmt of
  Apply gate qubits -> [ColGate gate qubits]
  Measure q var -> [ColMeasure q var]
  If _ body -> collectOps body
  Repeat _ body -> collectOps body
  Init _ -> []
  Print _ -> []

renderQubitLine :: Int -> [CircuitOp] -> String
renderQubitLine q ops =
  "q" ++ show q ++ " " ++ intercalate " " (map (renderCell q) ops)

renderCell :: Int -> CircuitOp -> String
renderCell q (ColMeasure mq _)
  | q == mq = center "M" 5
  | otherwise = wire 5
renderCell q (ColGate gate qubits) = case (gate, qubits) of
  (CNOT, [c, t])
    | q == c -> center "*" 5
    | q == t -> center "X" 5
    | otherwise -> wire 5
  (_, [target])
    | q == target -> center (gateLabel gate) 5
    | otherwise -> wire 5
  _ -> center "?" 5

gateLabel :: Gate -> String
gateLabel H = "H"
gateLabel X = "X"
gateLabel Y = "Y"
gateLabel Z = "Z"
gateLabel CNOT = "X"
gateLabel (Phase _) = "P"

wire :: Int -> String
wire n = replicate n '-'

center :: String -> Int -> String
center label width =
  let len = length label
      pad = max 0 (width - len)
      left = pad `div` 2
      right = pad - left
   in replicate left '-' ++ label ++ replicate right '-'
