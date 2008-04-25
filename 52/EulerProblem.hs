module Main where

import Data.List

sortedDigits :: Int -> String
sortedDigits = sort . show

sameDigits :: [Int] -> Bool
sameDigits [] = True
sameDigits [_] = True
sameDigits (x:xs) = all (== (sortedDigits x)) (map sortedDigits xs)

answer = head [x | x <- [10..], sameDigits [2*x, 3*x, 4*x, 5*x, 6*x]]
main = print answer
