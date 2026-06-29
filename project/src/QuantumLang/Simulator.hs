module QuantumLang.Simulator
  ( Amplitudes,
    initialState,
    applyGate,
    measureQubit,
  )
where

import Data.Bits (Bits (..))
import Data.Complex (Complex (..), magnitude, mkPolar)
import QuantumLang.Types (Gate (..))
import System.Random (randomIO)

type Amplitudes = [Complex Double]

initialState :: Int -> Amplitudes
initialState n
  | n < 0 = error "initialState: negative qubit count"
  | otherwise = 1 : replicate (2 ^ n - 1) (0 :+ 0)

applyGate :: Int -> Gate -> [Int] -> Amplitudes -> Either String Amplitudes
applyGate numQubits gate qubits amplitudes =
  case (gate, qubits) of
    (H, [q]) -> check1 q $ apply1Qubit numQubits q hMatrix amplitudes
    (X, [q]) -> check1 q $ apply1Qubit numQubits q xMatrix amplitudes
    (Y, [q]) -> check1 q $ apply1Qubit numQubits q yMatrix amplitudes
    (Z, [q]) -> check1 q $ apply1Qubit numQubits q zMatrix amplitudes
    (Phase theta, [q]) -> check1 q $ apply1Qubit numQubits q (phaseMatrix theta) amplitudes
    (CNOT, [control, target]) -> applyCNOT numQubits control target amplitudes
    _ -> Left $ "invalid gate application: " ++ show gate ++ " " ++ show qubits
  where
    check1 q amps
      | q < 0 || q >= numQubits = Left "qubit index out of range"
      | otherwise = Right amps

apply1Qubit :: Int -> Int -> [[Complex Double]] -> Amplitudes -> Amplitudes
apply1Qubit n q [[u00, u01], [u10, u11]] psi = go 0 psi
  where
    size = 2 ^ n
    targetMask = bit q
    go i acc
      | i >= size = acc
      | not (testBit i q) =
          let j = i .|. targetMask
              a0 = acc !! i
              a1 = acc !! j
              acc' =
                setAt i (u00 * a0 + u01 * a1) $
                  setAt j (u10 * a0 + u11 * a1) acc
           in go (i + 1) acc'
      | otherwise = go (i + 1) acc
apply1Qubit _ _ _ _ = error "apply1Qubit: malformed gate matrix"

applyCNOT :: Int -> Int -> Int -> Amplitudes -> Either String Amplitudes
applyCNOT n control target psi
  | control == target = Left "CNOT control and target must differ"
  | control < 0 || target < 0 || control >= n || target >= n =
      Left "CNOT qubit index out of range"
  | otherwise = Right (go 0 psi)
  where
    size = 2 ^ n
    targetMask = bit target
    go i acc
      | i >= size = acc
      | testBit i control && not (testBit i target) =
          let j = i .|. targetMask
           in go (i + 1) (swapAt i j acc)
      | otherwise = go (i + 1) acc

measureQubit :: Int -> Int -> Amplitudes -> IO (Int, Amplitudes)
measureQubit n q psi = do
  let p0 =
        sum
          [ magnitude (psi !! i) ^ (2 :: Int)
          | i <- [0 .. 2 ^ n - 1],
            not (testBit i q)
          ]
  r <- randomIO
  let outcome = if r < p0 then 0 else 1
      collapsed = normalize (collapseQubit q outcome psi)
  pure (outcome, collapsed)

collapseQubit :: Int -> Int -> Amplitudes -> Amplitudes
collapseQubit q outcome psi =
  [ if bitValue i == outcome then psi !! i else 0 :+ 0
  | i <- [0 .. length psi - 1]
  ]
  where
    bitValue i = if testBit i q then 1 else 0

normalize :: Amplitudes -> Amplitudes
normalize xs =
  let total = sum (map ((^(2 :: Int)) . magnitude) xs)
   in if total == 0
        then xs
        else map (/ (sqrt total :+ 0)) xs

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ x : drop (i + 1) xs

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs =
  let xi = xs !! i
      xj = xs !! j
   in setAt j xi (setAt i xj xs)

hMatrix :: [[Complex Double]]
hMatrix =
  let s = 1 / sqrt 2
   in [[s :+ 0, s :+ 0], [s :+ 0, (-s) :+ 0]]

xMatrix :: [[Complex Double]]
xMatrix = [[0 :+ 0, 1 :+ 0], [1 :+ 0, 0 :+ 0]]

yMatrix :: [[Complex Double]]
yMatrix = [[0 :+ 0, 0 :+ (-1)], [0 :+ 1, 0 :+ 0]]

zMatrix :: [[Complex Double]]
zMatrix = [[1 :+ 0, 0 :+ 0], [0 :+ 0, (-1) :+ 0]]

phaseMatrix :: Double -> [[Complex Double]]
phaseMatrix theta = [[1 :+ 0, 0 :+ 0], [0 :+ 0, mkPolar 1 theta]]
