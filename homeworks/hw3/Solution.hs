module Main where

import Control.Monad (foldM, guard)
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- ALL THE TESTS WERE WRITTEN BY AI 

-- 1. Maybe Monad - Maze navigation

type Pos = (Int, Int)

data Dir = N | S | E | W
  deriving (Eq, Ord, Show)

type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  neighbours <- Map.lookup pos maze
  Map.lookup dir neighbours

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze = foldM (move maze)

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = do
  (_, trace) <- foldM step (start, [start]) dirs
  pure trace
  where
    step :: (Pos, [Pos]) -> Dir -> Maybe (Pos, [Pos])
    step (current, trace) dir = do
      next <- move maze current dir
      pure (next, trace ++ [next])

-- Tests for Task 1
maze1 :: Maze
maze1 =
  Map.fromList
    [ ((0, 0), Map.fromList [(E, (1, 0)), (S, (0, 1))]),
      ((1, 0), Map.fromList [(W, (0, 0))]),
      ((0, 1), Map.fromList [(N, (0, 0)), (E, (1, 1))]),
      ((1, 1), Map.fromList [(W, (0, 1))])
    ]

testMove1 :: Bool
testMove1 = move maze1 (0, 0) E == Just (1, 0)

testMoveBlocked :: Bool
testMoveBlocked = move maze1 (0, 0) N == Nothing

testFollowPath1 :: Bool
testFollowPath1 = followPath maze1 (0, 0) [S, E] == Just (1, 1)

testFollowPathBlocked :: Bool
testFollowPathBlocked = followPath maze1 (0, 0) [E, E] == Nothing

testSafePath1 :: Bool
testSafePath1 = safePath maze1 (0, 0) [S, E] == Just [(0, 0), (0, 1), (1, 1)]

testSafePathBlocked :: Bool
testSafePathBlocked = safePath maze1 (0, 0) [E, E] == Nothing

-- 2. Decoding a message

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- Tests for Task 2
key1 :: Key
key1 = Map.fromList [('a', 'x'), ('b', 'y'), ('c', 'z'), (' ', ' ')]

testDecrypt1 :: Bool
testDecrypt1 = decrypt key1 "ab c" == Just "xy z"

testDecryptMissing :: Bool
testDecryptMissing = decrypt key1 "abd" == Nothing

testDecryptWords1 :: Bool
testDecryptWords1 = decryptWords key1 ["ab", "c"] == Just ["xy", "z"]

testDecryptWordsMissing :: Bool
testDecryptWordsMissing = decryptWords key1 ["ab", "d"] == Nothing

-- 3. List Monad - Seating arrangements

type Guest = String

type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  arrangement <- permutationsM guests
  guard (validSeating arrangement conflicts)
  pure arrangement

permutationsM :: [a] -> [[a]]
permutationsM [] = pure []
permutationsM xs = do
  (picked, rest) <- pickOne xs
  others <- permutationsM rest
  pure (picked : others)

pickOne :: [a] -> [(a, [a])]
pickOne [] = []
pickOne (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- pickOne xs]

validSeating :: [Guest] -> [Conflict] -> Bool
validSeating [] _ = True
validSeating [_] _ = True
validSeating arrangement conflicts =
  all noConflict neighbours
  where
    neighbours = zip arrangement (tail arrangement ++ [head arrangement])
    noConflict (a, b) = not (isConflict a b conflicts)

isConflict :: Guest -> Guest -> [Conflict] -> Bool
isConflict a b conflicts = (a, b) `elem` conflicts || (b, a) `elem` conflicts

-- Tests for Task 3
guests1 :: [Guest]
guests1 = ["Alice", "Bob", "Carol"]

conflicts1 :: [Conflict]
conflicts1 = [("Alice", "Bob")]

testSeatingsConflict :: Bool
testSeatingsConflict = null (seatings guests1 conflicts1)

guests2 :: [Guest]
guests2 = ["Alice", "Bob", "Carol", "Dave"]

conflicts2 :: [Conflict]
conflicts2 = [("Alice", "Bob")]

testSeatingsCount :: Bool
testSeatingsCount = length (seatings guests2 conflicts2) == 8

testSeatingsRoundCheck :: Bool
testSeatingsRoundCheck =
  all
    (\arr -> not (isConflict (head arr) (last arr) conflicts2))
    (seatings guests2 conflicts2)

-- 5. Writer Monad - Evaluator with simplification log

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  deriving (Eq, Show)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = pure (Lit n)
simplify (Neg e) = do
  e' <- simplify e
  simplifyRoot (Neg e')
simplify (Add a b) = do
  a' <- simplify a
  b' <- simplify b
  simplifyRoot (Add a' b')
simplify (Mul a b) = do
  a' <- simplify a
  b' <- simplify b
  simplifyRoot (Mul a' b')

simplifyRoot :: Expr -> Writer [String] Expr
simplifyRoot (Add (Lit 0) e) = do
  tell ["Add identity: 0 + e -> e"]
  pure e
simplifyRoot (Add e (Lit 0)) = do
  tell ["Add identity: e + 0 -> e"]
  pure e
simplifyRoot (Mul (Lit 1) e) = do
  tell ["Mul identity: 1 * e -> e"]
  pure e
simplifyRoot (Mul e (Lit 1)) = do
  tell ["Mul identity: e * 1 -> e"]
  pure e
simplifyRoot (Mul (Lit 0) _) = do
  tell ["Mul absorption: 0 * e -> 0"]
  pure (Lit 0)
simplifyRoot (Mul _ (Lit 0)) = do
  tell ["Mul absorption: e * 0 -> 0"]
  pure (Lit 0)
simplifyRoot (Neg (Neg e)) = do
  tell ["Double negation: -(-e) -> e"]
  pure e
simplifyRoot (Add (Lit a) (Lit b)) = do
  tell ["Add constant folding"]
  pure (Lit (a + b))
simplifyRoot (Mul (Lit a) (Lit b)) = do
  tell ["Mul constant folding"]
  pure (Lit (a * b))
simplifyRoot e = pure e

-- Tests for Task 5
expr1 :: Expr
expr1 = Add (Lit 0) (Mul (Lit 2) (Lit 3))

testSimplifyExpr1 :: Bool
testSimplifyExpr1 =
  let (result, logs) = runWriter (simplify expr1)
   in result == Lit 6 && length logs == 2

expr2 :: Expr
expr2 = Mul (Neg (Neg (Lit 5))) (Lit 1)

testSimplifyExpr2 :: Bool
testSimplifyExpr2 =
  let (result, logs) = runWriter (simplify expr2)
   in result == Lit 5 && length logs == 2

expr3 :: Expr
expr3 = Mul (Lit 0) (Add (Lit 10) (Lit 20))

testSimplifyExpr3 :: Bool
testSimplifyExpr3 =
  let (result, logs) = runWriter (simplify expr3)
   in result == Lit 0 && length logs == 2

-- Minimal test runner

assert :: String -> Bool -> IO ()
assert name ok =
  putStrLn $
    (if ok then "[PASS] " else "[FAIL] ") ++ name

main :: IO ()
main = do
  putStrLn "Task 1 tests:"
  assert "move simple" testMove1
  assert "move blocked" testMoveBlocked
  assert "followPath success" testFollowPath1
  assert "followPath blocked" testFollowPathBlocked
  assert "safePath success" testSafePath1
  assert "safePath blocked" testSafePathBlocked

  putStrLn "\nTask 2 tests:"
  assert "decrypt success" testDecrypt1
  assert "decrypt missing char" testDecryptMissing
  assert "decryptWords success" testDecryptWords1
  assert "decryptWords missing char" testDecryptWordsMissing

  putStrLn "\nTask 3 tests:"
  assert "seatings impossible (3 guests, one conflict)" testSeatingsConflict
  assert "seatings count (4 guests, one conflict)" testSeatingsCount
  assert "seatings enforce round-neighbour rule" testSeatingsRoundCheck

  putStrLn "\nTask 5 tests:"
  assert "simplify add identity + constant fold" testSimplifyExpr1
  assert "simplify double negation + mul identity" testSimplifyExpr2
  assert "simplify mul absorption + sub-fold" testSimplifyExpr3
