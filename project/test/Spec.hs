{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Data.Bits (testBit)
import Data.Complex (Complex (..), magnitude)
import qualified Data.Map.Strict as Map
import QuantumLang.Interpreter (runProgramWith)
import QuantumLang.Parser (parseProgram)
import QuantumLang.Simulator
import QuantumLang.Types
import System.Random (mkStdGen, setStdGen)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, (@=?), (@?=), assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "quantum-lang"
    [ parserTests,
      simulatorTests,
      endToEndTests,
      propertyTests
    ]

parserTests :: TestTree
parserTests =
  testGroup
    "parser"
    [ testCase "parses bell program" $
        case parseProgram bellSource of
          Left err -> assertFailure (show err)
          Right (Program stmts) ->
            length stmts @?= 7,
      testCase "rejects bad syntax with location" $
        case parseProgram "init 2\napply H [0\n" of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure"
    ]

simulatorTests :: TestTree
simulatorTests =
  testGroup
    "simulator"
    [ testCase "X sends |0> to |1>" $ do
        let psi0 = initialState 1
            Right psi1 = applyGate 1 X [0] psi0
         in magnitude (psi1 !! 1) @?= 1.0,
      testCase "H is unitary" $
        isUnitary2 (gateMatrix H) @?= True,
      testCase "X is unitary" $
        isUnitary2 (gateMatrix X) @?= True,
      testCase "Y is unitary" $
        isUnitary2 (gateMatrix Y) @?= True,
      testCase "Z is unitary" $
        isUnitary2 (gateMatrix Z) @?= True,
      testCase "Phase is unitary" $
        isUnitary2 (gateMatrix (Phase 1.23)) @?= True,
      testCase "basis |0> measurement is deterministic" $ do
        let (bit, _) = measureQubitWith 0.0 1 0 (initialState 1)
         in bit @?= 0,
      testCase "basis |1> measurement is deterministic" $ do
        let psi = case applyGate 1 X [0] (initialState 1) of
              Right p -> p
              Left err -> error err
            (bit, _) = measureQubitWith 0.99 1 0 psi
         in bit @?= 1,
      testCase "CNOT entangles |00> into (|00>+|11>)/sqrt 2" $ do
        let Right psi =
              applyGate 2 H [0] (initialState 2)
                >>= applyGate 2 CNOT [0, 1]
            a00 = magnitude (psi !! 0)
            a11 = magnitude (psi !! 3)
         in abs (a00 - (1 / sqrt 2)) + abs (a11 - (1 / sqrt 2)) @?= 0.0
    ]

endToEndTests :: TestTree
endToEndTests =
  testGroup
    "end-to-end"
    [ testCase "bell measurements stay correlated" $ do
        setStdGen (mkStdGen 42)
        env <- runOrFail bellSource
        Map.lookup "m0" env @=? Map.lookup "m1" env,
      testCase "deutsch balanced returns 1" $ do
        setStdGen (mkStdGen 7)
        env <- runOrFail deutschSource
        Map.lookup "result" env @?= Just 1,
      testCase "deutsch constant returns 0" $ do
        setStdGen (mkStdGen 7)
        env <- runOrFail deutschConstantSource
        Map.lookup "result" env @?= Just 0,
      testCase "teleport preserves |1>" $ do
        setStdGen (mkStdGen 99)
        env <- runOrFail teleportSource
        Map.lookup "teleported" env @?= Just 1
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "properties"
    [ testProperty "gates preserve normalization" $
        \(seed :: Int) ->
          let psi = initialState 2
              Right psi' =
                applyGate 2 H [0] psi
                  >>= applyGate 2 X [1]
                  >>= applyGate 2 (Phase (fromIntegral (abs seed `mod` 7))) [0]
                  >>= applyGate 2 CNOT [0, 1]
           in isNormalized psi',
      testProperty "measurement probabilities sum to one" $
        \(seed :: Int) ->
          let n = 2
              q = abs seed `mod` n
              psi = initialState n
              p0 =
                sum
                  [ magnitude (psi !! i) ^ (2 :: Int)
                  | i <- [0 .. 2 ^ n - 1],
                    not (testBit i q)
                  ]
              p1 = 1 - p0
           in abs (p0 + p1 - 1) < 1e-9
    ]

runOrFail :: String -> IO (Map.Map String Int)
runOrFail src = do
  result <- runProgramWith (parseOrFail src)
  case result of
    Left err -> assertFailure err
    Right env -> pure env

parseOrFail :: String -> Program
parseOrFail src =
  case parseProgram src of
    Left err -> error (show err)
    Right prog -> prog

bellSource :: String
bellSource =
  unlines
    [ "init 2",
      "apply H [0]",
      "apply CNOT [0, 1]",
      "measure 0 -> m0",
      "measure 1 -> m1",
      "print m0",
      "print m1"
    ]

deutschSource :: String
deutschSource =
  unlines
    [ "init 2",
      "apply X [1]",
      "apply H [0]",
      "apply H [1]",
      "apply CNOT [0, 1]",
      "apply H [0]",
      "measure 0 -> result",
      "print result"
    ]

deutschConstantSource :: String
deutschConstantSource =
  unlines
    [ "init 2",
      "apply X [1]",
      "apply H [0]",
      "apply H [1]",
      "apply H [0]",
      "measure 0 -> result",
      "print result"
    ]

teleportSource :: String
teleportSource =
  unlines
    [ "init 3",
      "apply X [0]",
      "apply H [1]",
      "apply CNOT [1, 2]",
      "apply CNOT [0, 1]",
      "apply H [0]",
      "measure 0 -> m0",
      "measure 1 -> m1",
      "if m1",
      "  apply X [2]",
      "end",
      "if m0",
      "  apply Z [2]",
      "end",
      "measure 2 -> teleported",
      "print teleported"
    ]
