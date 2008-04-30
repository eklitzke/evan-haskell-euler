module Main where

import Data.Array
import Primes

myOdds :: [Int]
myOdds = [x | x <- [1000..], odd x]

diagne, diagnw, diagsw, diagse :: Integral a => [a]
diagne = [x^2 - x + 1 | x <- [2..], even x]
diagnw = [x^2 + 1 | x <- [2..], even x]
diagsw = [x^2 + x + 1 | x <- [2..], even x]
diagse = [x^2 | x <- [3..], odd x]

-- Returns the numbers in the square with a side of length n
square :: Integral a => Int -> [a]
square n = 1 : (concat (map (take n') [diagne, diagnw, diagsw, diagse]))
    where n' = (n - 1) `div` 2

squareArray :: Integral a => Array Int [a]
squareArray = array (3,30000) [(n, square n) | n <- [3..30000]]

primesRatio :: Integral a => [a] -> Float
primesRatio xs = (fromIntegral n) / (fromIntegral ln)
    where
    n = length [x | x <- xs, isPrime x]
    ln = length xs

magic b = (primesRatio b) < 0.10

halfInt :: Integral a => a -> a -> a
halfInt a b = (a + b) `div` 2

findAnswer :: Bool -> Int -> Int -> Int
findAnswer False a b =
    if magic (squareArray!b)
        then findAnswer True a b
        else findAnswer False b (2*b)
findAnswer True a b
    | a == b = a
    | a == (b - 1) = if magic (squareArray!a) then a else b
    | otherwise = if magic (squareArray!h)
        then findAnswer True a h
        else findAnswer True h b
        where
      h = halfInt a b

answer = findAnswer False 3 7
main = print answer
