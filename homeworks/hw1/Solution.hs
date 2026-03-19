{-# LANGUAGE BangPatterns #-}

module Solution where

import Data.List (nub)

-- 1) Goldbach Pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
  | n < 4 || odd n = []
  | otherwise =
      [ (p, q)
      | p <- [2 .. n]
      , q <- [p .. n]
      , p + q == n
      , isPrime p
      , isPrime q
      ]

-- 2) Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs =
  nub
    [ (x, y)
    | x <- xs
    , y <- xs
    , x < y
    , gcd x y == 1
    ]

-- 3) Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n
  | n < 2 = []
  | otherwise = sieve [2 .. n]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = n `elem` primesTo n

-- 4) Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b
  | null a || null b = []
  | any null a || any null b = []
  | p /= length b = error "matMul: incompatible dimensions"
  | otherwise =
      [ [ sum [a !! i !! k * b !! k !! j | k <- [0 .. p - 1]]
        | j <- [0 .. n - 1]
        ]
      | i <- [0 .. m - 1]
      ]
  where
    m = length a
    p = length (head a)
    n = length (head b)

-- 5) k-permutations
permutations :: Int -> [a] -> [[a]]
permutations k _
  | k < 0 = []
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs =
  [ y : ys
  | (y, rest) <- picks xs
  , ys <- permutations (k - 1) rest
  ]
  where
    picks :: [a] -> [(a, [a])]
    picks [] = []
    picks (z:zs) = (z, zs) : [(w, z : ws) | (w, ws) <- picks zs]

-- 6) Hamming Numbers
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | x > y = y : merge (x:xs) ys
  | otherwise = x : merge xs ys

hamming :: [Integer]
hamming = 1 : merge (map (2 *) hamming) (merge (map (3 *) hamming) (map (5 *) hamming))

-- 7) Integer Power with Bang Patterns
power :: Int -> Int -> Int
power _ e | e < 0 = error "power: negative exponent not supported for Int result"
power b e = go 1 e
  where
    go :: Int -> Int -> Int
    go !acc 0 = acc
    go !acc n = go (acc * b) (n - 1)

-- 8) Running Maximum
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "listMaxSeq: empty list"
listMaxSeq (x:xs) = go x xs
  where
    go acc [] = acc
    go acc (y:ys) =
      let acc' = max acc y
       in acc' `seq` go acc' ys

listMaxBang :: [Int] -> Int
listMaxBang [] = error "listMaxBang: empty list"
listMaxBang (x:xs) = go x xs
  where
    go !acc [] = acc
    go !acc (y:ys) = go (max acc y) ys

-- 9) Infinite Prime Stream
primes :: [Int]
primes = sieve [2 ..]

isPrimeInfinite :: Int -> Bool
isPrimeInfinite n
  | n < 2 = False
  | otherwise = go primes
  where
    go (p:ps)
      | p * p > n = True
      | n `mod` p == 0 = n == p
      | otherwise = go ps
    go [] = False

-- 10) Strict Accumulation and Space Leaks
-- (a) Lazy version
meanLazy :: [Double] -> Double
meanLazy [] = error "meanLazy: empty list"
meanLazy xs =
  let (s, n) = go 0 0 xs
   in s / fromIntegral n
  where
    go :: Double -> Int -> [Double] -> (Double, Int)
    go s n [] = (s, n)
    go s n (y:ys) = go (s + y) (n + 1) ys

-- (b) Strict version with bang patterns on both components.
mean :: [Double] -> Double
mean [] = error "mean: empty list"
mean xs =
  let (s, n) = go 0 0 xs
   in s / fromIntegral n
  where
    go :: Double -> Int -> [Double] -> (Double, Int)
    go !s !n [] = (s, n)
    go !s !n (y:ys) =
      let !s' = s + y
          !n' = n + 1
       in go s' n' ys

-- Strictness should be applied to the individual components because a bang on a pair only forces the constructor(just a note for myself).

-- (c) Mean and variance in one pass.
meanVariance :: [Double] -> (Double, Double)
meanVariance [] = error "meanVariance: empty list"
meanVariance xs =
  let (s, s2, n) = go 0 0 0 xs
      mu = s / fromIntegral n
      var = (s2 / fromIntegral n) - (mu * mu)
   in (mu, var)
  where
    go :: Double -> Double -> Int -> [Double] -> (Double, Double, Int)
    go !s !s2 !n [] = (s, s2, n)
    go !s !s2 !n (y:ys) =
      let !s' = s + y
          !s2' = s2 + y * y
          !n' = n + 1
       in go s' s2' n' ys



-- Tests written by AI
main :: IO ()
main = do
  print $ goldbachPairs 10
  print $ coprimePairs [1,2,3,4,5]
  print $ primesTo 30
  print $ matMul [[1,2],[3,4]] [[5,6],[7,8]]
  print $ permutations 2 [1,2,3]
  print $ take 15 hamming
  print $ power 2 10
  print $ listMaxSeq [3,1,9,2]
  print $ listMaxBang [3,1,9,2]
  print $ take 20 primes
  print $ isPrimeInfinite 97
  print $ mean [1,2,3,4,5]
  print $ meanVariance [1,2,3,4,5]