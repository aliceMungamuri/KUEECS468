-- EECS 468 Assignment 7 
-- Author: Alice J Mungamuri
-- April 21, 2025
-- Description: Defines  Haskell functions for list processing/ combinatorial counting
-- Inputs: integers, lists, and tuples
-- Outputs:lists or integers
-- Sources: Stack overfliw, ChatGPT (OpenAI), Lecture slides, 

-- replicate' :: Int -> a -> [a] - im assuming that that function adds the ' because replicate is the name of the built in function
-- not hardcoded because i uded the variable n
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]] --  for all the numbers in 1 to n - the x is in the list = Stack overflow


perfects :: Int -> [Int]-- gives a list of all perfect numbers up to a given limit n (variable)
perfects n = [x | x <- [2..n], sum [d | d <- [1..x-1], x `mod` d == 0] == x]
--  x is perfect if the sum of its divisors excluding itself equals x -- I had to look that up


find :: Eq a => a -> [(a,b)] -> [b]-- all values associated with a key in a list of key-value pairs
find k t = [v | (k', v) <- t, k == k'] --  each (key,value), include value if key matches


positions :: Eq a => a -> [a] -> [Int]-- positions of an element in a list using find and zip
positions x xs = find x (zip xs [0..]) --  each element with its index and then itt will use find


scalarproduct :: [Int] -> [Int] -> Int--  dot product 

scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]-- * the elements and adds them


factorial :: Int -> Int -- factorial will be an int
factorial 0 = 1 -- base
factorial n = n * factorial (n - 1) -- recursive

dodb :: Int -> Int -> Int -> Int
dodb n k m
  | n /= k * m = 0 -- Return 0 if total objects doesn't match
  | otherwise = factorial n `div` (factorial m ^ k) -- it will only work if n = k * m

-- Uses combinations with repetition: C(n + k - 1, n)
iodb :: Int -> Int -> Int -- return int

iodb n k = factorial (n + k - 1) `div` (factorial n * factorial (k - 1)) -- n things into k things with m number n's per k


stirling :: Int -> Int -> Int ----  stirling numbers of the second kind to distribute n distinguishable objects into k indistinguishable boxes

stirling n k -- ecurrence relation: S(n, k) = k * S(n-1, k) + S(n-1, k-1)
  | n == k = 1
  | k == 0 || k > n = 0
  | otherwise = k * stirling (n - 1) k + stirling (n - 1) (k - 1) -- S(n, k) = k * S(n-1, k) + S(n-1, k-1)

doib :: Int -> Int -> Int
doib = stirling -- Just aliasing for clarity

partition :: Int -> Int -> Int -- int to int to return int
partition n k -- Uses number of integer partitions (p_k(n)) of n into at most k parts

  | n == 0 = 1
  | k == 0 || n < 0 = 0
  | otherwise = partition (n - k) k + partition n (k - 1) -- Based on recurrence: p_k(n) = p_k(n-k) + p_{k-1}(n)

ioib :: Int -> Int -> Int
ioib = partition
