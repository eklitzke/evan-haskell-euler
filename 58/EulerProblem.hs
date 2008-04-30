module Main where

import Primes

myOdds :: [Int]
myOdds = [x | x <- [1000..], odd x]

diagne, diagnw, diagsw, diagse :: [Integer]
diagne = [x^2 - x + 1 | x <- [2..], even x]
diagnw = [x^2 + 1 | x <- [2..], even x]
diagsw = [x^2 + x + 1 | x <- [2..], even x]
diagse = [x^2 | x <- [3..], odd x]

square :: Int -> [Integer]
square n = concat (map (take n') [diagne, diagnw, diagsw, diagse])
    where n' = (n - 1) `div` 2

squares :: [(Int, [Integer])]
squares = [(o, square o) | o <- myOdds]

primesRatio :: Integral a => [a] -> Float
primesRatio xs = (fromIntegral n) / (fromIntegral ln)
    where
    n = length [x | x <- xs, isPrime x]
    ln = length xs

magic (a, b) = (primesRatio b) > 0.1

answer = fst $ head $ dropWhile magic squares
main = print answer
