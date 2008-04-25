module Main where

import EulerMath
import Data.List

numFactors :: Int -> Int
numFactors = length . nub . factor

magic :: Int -> [Int] -> Bool
magic _ [] = True
magic n (x:xs) = (numFactors x == n) && (magic n xs)

answer = head [x | x <- [2..], magic 4 [x, x+1, x+2, x+3]]

main = print answer
