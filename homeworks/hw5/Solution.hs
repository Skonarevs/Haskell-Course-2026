module Main where

import Control.Monad (unless, when)
import Control.Monad.State (State, StateT, evalState, execState, get, gets, modify, put)
import Control.Monad.IO.Class (liftIO)
import Data.List (minimum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- 1. Stack machine (State Monad)

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving (Eq, Show)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)
execInstr POP = do
  s <- get
  case s of
    _ : xs -> put xs
    [] -> pure ()
execInstr DUP = do
  s <- get
  case s of
    x : xs -> put (x : x : xs)
    [] -> pure ()
execInstr SWAP = do
  s <- get
  case s of
    y : x : xs -> put (x : y : xs)
    _ -> pure ()
execInstr ADD = do
  s <- get
  case s of
    y : x : xs -> put (x + y : xs)
    _ -> pure ()
execInstr MUL = do
  s <- get
  case s of
    y : x : xs -> put (x * y : xs)
    _ -> pure ()
execInstr NEG = do
  s <- get
  case s of
    x : xs -> put (negate x : xs)
    [] -> pure ()

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []

-- 2. Expression evaluator with variable bindings

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr
  | Seq Expr Expr
  deriving (Eq, Show)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = pure n
eval (Var name) = gets (Map.! name)
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Neg e) = negate <$> eval e
eval (Assign name e) = do
  v <- eval e
  modify (Map.insert name v)
  pure v
eval (Seq e1 e2) = eval e1 >> eval e2

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty

-- 3. Memoised edit (Levenshtein) distance

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get
  case Map.lookup (i, j) cache of
    Just d -> pure d
    Nothing -> do
      d <-
        if i == 0
          then pure j
          else
            if j == 0
              then pure i
              else
                if xs !! (i - 1) == ys !! (j - 1)
                  then editDistM xs ys (i - 1) (j - 1)
                  else do
                    del <- editDistM xs ys (i - 1) j
                    ins <- editDistM xs ys i (j - 1)
                    sub <- editDistM xs ys (i - 1) (j - 1)
                    pure (1 + minimum [del, ins, sub])
      modify (Map.insert (i, j) d)
      pure d

editDistance :: String -> String -> Int
editDistance xs ys =
  evalState (editDistM xs ys (length xs) (length ys)) Map.empty

-- 4–6. Treasure Hunters (WIP — scaffolding only, INTERFACE CREATED USING AI)

data LocationKind
  = Plain
  | DecisionPoint
  | Obstacle
  | SmallTreasure
  | Trap
  | Goal
  deriving (Eq, Show)

data BoardCell = BoardCell
  { cellLabel :: String
  , cellKind :: LocationKind
  }
  deriving (Eq, Show)

data GameState = GameState
  { gsPosition :: Int
  , gsEnergy :: Int
  , gsScore :: Int
  , gsBoard :: [BoardCell]
  }
  deriving (Eq, Show)

type AdventureGame a = StateT GameState IO a

initialGameState :: GameState
initialGameState =
  GameState
    { gsPosition = 0
    , gsEnergy = 20
    , gsScore = 0
    , gsBoard =
        [ BoardCell "Camp" Plain
        , BoardCell "Fork" DecisionPoint
        , BoardCell "Bog" Obstacle
        , BoardCell "Chest" SmallTreasure
        , BoardCell "Pit" Trap
        , BoardCell "Treasure" Goal
        ]
    }

getDiceRoll :: IO Int
getDiceRoll = promptIntInRange "Enter dice roll (1–6): " 1 6

displayGameState :: GameState -> IO ()
displayGameState gs = do
  let cell = gsBoard gs !! min (gsPosition gs) (length (gsBoard gs) - 1)
  putStrLn "─── Treasure Hunters ───"
  putStrLn $ "Position: " ++ show (gsPosition gs) ++ " / " ++ show (length (gsBoard gs) - 1)
  putStrLn $ "Location: " ++ cellLabel cell ++ " (" ++ show (cellKind cell) ++ ")"
  putStrLn $ "Energy:   " ++ show (gsEnergy gs)
  putStrLn $ "Score:    " ++ show (gsScore gs)
  putStrLn "────────────────────────"

getPlayerChoice :: [String] -> IO String
getPlayerChoice [] = pure ""
getPlayerChoice opts = do
  putStrLn "Choose a path:"
  mapM_ (\(i, o) -> putStrLn $ "  " ++ show i ++ ") " ++ o) (zip [1 ..] opts)
  idx <- promptIntInRange "Your choice (number): " 1 (length opts)
  pure (opts !! (idx - 1))

promptIntInRange :: String -> Int -> Int -> IO Int
promptIntInRange prompt lo hi = go
  where
    go = do
      putStr prompt
      line <- getLine
      case reads line of
        [(n, "")] | n >= lo && n <= hi -> pure n
        _ -> do
          putStrLn $ "Please enter an integer from " ++ show lo ++ " to " ++ show hi ++ "."
          go

movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
  gs <- get
  let spaces = max 1 (min 6 roll)
  let nextPos = min (length (gsBoard gs) - 1) (gsPosition gs + spaces)
  let newEnergy = gsEnergy gs - 1
  put gs {gsPosition = nextPos, gsEnergy = newEnergy}
  pure spaces

makeDecision :: [String] -> AdventureGame String
makeDecision opts = liftIO (getPlayerChoice opts)

handleLocation :: AdventureGame Bool
handleLocation = do
  gs <- get
  let cell = gsBoard gs !! gsPosition gs
  case cellKind cell of
    Goal -> pure True
    DecisionPoint -> do
      _ <- makeDecision ["Left path (TODO)", "Right path (TODO)"]
      -- path choice does not affect position yet
      pure False
    Obstacle -> do
      put gs {gsEnergy = max 0 (gsEnergy gs - 2)}
      pure False
    SmallTreasure -> do
      put gs {gsScore = gsScore gs + 5}
      pure False
    Trap -> do
      put gs {gsScore = max 0 (gsScore gs - 3)}
      pure False
    Plain -> pure False

playTurn :: AdventureGame Bool
playTurn = do
  gs <- get
  liftIO $ displayGameState gs
  if gsEnergy gs <= 0
    then do
      liftIO $ putStrLn "Out of energy — game over."
      pure True
    else do
      roll <- liftIO getDiceRoll
      _ <- movePlayer roll
      reachedGoal <- handleLocation
      gs' <- get
      let outOfEnergy = gsEnergy gs' <= 0
      pure (reachedGoal || outOfEnergy)

playGame :: AdventureGame ()
playGame = do
  liftIO $ putStrLn "Starting Treasure Hunters (work in progress)…"
  let loop = do
        over <- playTurn
        unless over loop
  loop
  liftIO $ putStrLn "Thanks for playing!"

demoGameOneTurn :: IO ()
demoGameOneTurn = do
  putStrLn "\n[Game demo] One automated turn on the stub board:"
  let gs = initialGameState {gsEnergy = 5}
  displayGameState gs
  putStrLn "(Full playGame loop still needs board paths, traps wiring, and win rules.)"



-- ALL TESTS CREATED USING AI
-- Tests — State Monad (tasks 1–3)

assert :: String -> Bool -> IO ()
assert name ok =
  putStrLn $
    if ok then "[PASS] " else "[FAIL] "
      ++ name

-- Task 1
progAdd :: [Instr]
progAdd = [PUSH 2, PUSH 3, ADD]

progSwapAdd :: [Instr]
progSwapAdd = [PUSH 1, PUSH 2, SWAP, ADD]

testPushAdd :: Bool
testPushAdd = runProg progAdd == [5]

testPopOnEmpty :: Bool
testPopOnEmpty = runProg [POP] == []

testAddInsufficient :: Bool
testAddInsufficient = runProg [PUSH 1, ADD] == [1]

testMulNeg :: Bool
testMulNeg = runProg [PUSH 3, PUSH 4, MUL, NEG] == [-12]

testSwapAdd :: Bool
testSwapAdd = runProg progSwapAdd == [3]

testDup :: Bool
testDup = runProg [PUSH 7, DUP] == [7, 7]

testEmptyProg :: Bool
testEmptyProg = runProg [] == []

-- Task 2
exprAssignSeq :: Expr
exprAssignSeq =
  Seq (Assign "x" (Num 10)) (Add (Var "x") (Assign "y" (Mul (Var "x") (Num 2))))

testRunEvalAssign :: Bool
testRunEvalAssign = runEval exprAssignSeq == 30

testRunEvalNeg :: Bool
testRunEvalNeg = runEval (Neg (Num 7)) == -7

testRunEvalAdd :: Bool
testRunEvalAdd = runEval (Add (Num 2) (Num 3)) == 5

-- Task 3
testEditEmpty :: Bool
testEditEmpty = editDistance "" "" == 0

testEditInsert :: Bool
testEditInsert = editDistance "" "abc" == 3

testEditDelete :: Bool
testEditDelete = editDistance "abc" "" == 3

testEditSame :: Bool
testEditSame = editDistance "haskell" "haskell" == 0

testEditKitten :: Bool
testEditKitten = editDistance "kitten" "sitting" == 3

testEditSubstitution :: Bool
testEditSubstitution = editDistance "flaw" "lawn" == 2

-- =============================================================================
-- Main
-- =============================================================================

main :: IO ()
main = do
  putStrLn "=== HW5: State Monad tests ===\n"

  putStrLn "Task 1 (stack machine):"
  assert "PUSH + ADD" testPushAdd
  assert "POP on empty stack (no-op)" testPopOnEmpty
  assert "ADD with one operand (skipped)" testAddInsufficient
  assert "MUL then NEG" testMulNeg
  assert "SWAP + ADD" testSwapAdd
  assert "DUP duplicates top" testDup
  assert "empty program" testEmptyProg

  putStrLn "\nTask 2 (expression evaluator):"
  assert "Add two numbers" testRunEvalAdd
  assert "Negate" testRunEvalNeg
  assert "Assign + Seq + Var" testRunEvalAssign

  putStrLn "\nTask 3 (memoised edit distance):"
  assert "empty / empty" testEditEmpty
  assert "insertions only" testEditInsert
  assert "deletions only" testEditDelete
  assert "identical strings" testEditSame
  assert "kitten -> sitting" testEditKitten
  assert "flaw -> lawn" testEditSubstitution

  demoGameOneTurn

  putStrLn "\nTo play the stub game interactively, call playGame from ghci:"
  putStrLn "  runStateT playGame initialGameState"
