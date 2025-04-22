-- EECS 468 Assignment 7 
-- Author: Alice J Mungamuri
-- April 21, 2025
-- Description: Defines  Haskell functions for list processing/ combinatorial counting
-- Inputs: integers, lists, and tuples
-- Outputs:lists or integers
-- Sources: Stack overfliw, ChatGPT (OpenAI), Lecture slides, 

-- replicate' :: Int -> a -> [a]
-- Replicates an element x, n times using list comprehension
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]] -- For each number in 1 to n, include x in the list

-- perfects :: Int -> [Int]
-- Returns a list of all perfect numbers up to a given limit n
perfects :: Int -> [Int]
perfects n = [x | x <- [2..n], sum [d | d <- [1..x-1], x `mod` d == 0] == x]
-- A number x is perfect if the sum of its divisors excluding itself equals x

-- find :: Eq a => a -> [(a,b)] -> [b]
-- Looks up all values associated with a key in a list of key-value pairs
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k'] -- For each (key,value), include value if key matches

-- positions :: Eq a => a -> [a] -> [Int]
-- Finds all positions of an element in a list using find and zip
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..]) -- Pairs each element with its index, then uses find

-- scalarproduct :: [Int] -> [Int] -> Int
-- Calculates dot product using zip and list comprehension
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]
-- Multiplies corresponding elements and sums them

-- dodb :: Int -> Int -> Int -> Int
-- Distributes n distinguishable objects into k distinguishable boxes with m objects per box
-- Uses multinomial coefficient: n! / (m! * m! * ... * m!) where there are k groups of m
-- Only valid if n = k * m
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

dodb :: Int -> Int -> Int -> Int
dodb n k m
  | n /= k * m = 0 -- Return 0 if total objects doesn't match
  | otherwise = factorial n `div` (factorial m ^ k)

-- iodb :: Int -> Int -> Int
-- Counts the ways to distribute n indistinguishable objects into k distinguishable boxes
-- Uses combinations with repetition: C(n + k - 1, n)
iodb :: Int -> Int -> Int
iodb n k = factorial (n + k - 1) `div` (factorial n * factorial (k - 1))

-- doib :: Int -> Int -> Int
-- Uses Stirling numbers of the second kind to distribute n distinguishable objects into k indistinguishable boxes
-- Based on recurrence relation: S(n, k) = k * S(n-1, k) + S(n-1, k-1)
stirling :: Int -> Int -> Int
stirling n k
  | n == k = 1
  | k == 0 || k > n = 0
  | otherwise = k * stirling (n - 1) k + stirling (n - 1) (k - 1)

doib :: Int -> Int -> Int
doib = stirling -- Just aliasing for clarity

-- ioib :: Int -> Int -> Int
-- Uses number of integer partitions (p_k(n)) of n into at most k parts
-- Based on recurrence: p_k(n) = p_k(n-k) + p_{k-1}(n)
partition :: Int -> Int -> Int
partition n k
  | n == 0 = 1
  | k == 0 || n < 0 = 0
  | otherwise = partition (n - k) k + partition n (k - 1)

ioib :: Int -> Int -> Int
ioib = partition
