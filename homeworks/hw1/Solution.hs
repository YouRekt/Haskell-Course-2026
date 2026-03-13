module Solution (goldbachPairs) where

import GHC.Base qualified as Ex

-- Ex.1

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
  -- \| n >= 4 && even n = [(p, q) | p <- [2 .. n], q <- [2 .. n], p <= q, p + q == n, isPrime p, isPrime q]
  | n >= 4 && even n = [(p, q) | p <- [2 .. div n 2], let q = n - p, isPrime p, isPrime q]
  | otherwise = []

-- Ex.2

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs xs = [(x, y) | x <- xs, y <- xs, x < y, gcd x y == 1]

-- Ex.3

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x : xs)
  | x >= 2 = x : sieve (filter (\y -> mod y x /= 0) xs)
  | otherwise = sieve xs

primesTo :: Int -> [Int]
primesTo n
  | n >= 2 = sieve [2 .. n]
  | otherwise = []

containsInt :: Int -> [Int] -> Bool
containsInt _ [] = False
containsInt n (x : xs)
  | n == x = True
  | otherwise = containsInt n xs

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = containsInt n (primesTo n)

-- Ex.4

matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b =
  [[sum [a !! i !! k * b !! k !! j | k <- [0 .. p - 1]] | j <- [0 .. n - 1]] | i <- [0 .. m - 1]]
  where
    m = length a
    (rowA : _) = a
    (rowB : _) = b
    p = length rowA
    n = length rowB

-- Ex.5

permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k lst = [x : xs | (x, remaining) <- picks lst, xs <- permutations (k - 1) remaining]
  where
    picks :: [a] -> [(a, [a])]
    picks [] = []
    picks (y : ys) = (y, ys) : [(z, y : zs) | (z, zs) <- picks ys]
