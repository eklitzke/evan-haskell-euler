module Main where

import Data.List
import Primes

truncationsLeft :: Eq a => [a] -> [[a]]
truncationsLeft [] = []
truncationsLeft (x:xs) = (x:xs) : (truncationsLeft xs)

-- strictly speaking Eq shouldn't be necessary but I want to use nub
truncations :: Eq a => [a] -> [[a]]
truncations xs = nub $ (truncationsLeft xs) ++ (map reverse (truncationsLeft (reverse xs)))

answer = sum $ take 11 [x | x <- [11..], all isPrime (map read (truncations (show x)))]

main = print $ answer
