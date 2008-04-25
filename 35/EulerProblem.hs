module Main where

import Data.List
import Primes

readIntegral :: (Read a, Integral a) => String -> a
readIntegral = read

rotateOnce :: [a] -> [a]
rotateOnce [] = []
rotateOnce (x:xs) = xs ++ [x]

rotations n = (map readIntegral (getRotations (ln - 1) sn))
    where
    sn = show n
    ln = length sn
    getRotations 0  l = []
    getRotations n' l = let rl = rotateOnce l in rl : (getRotations (n'-1) rl)

answer = length [x | x <- [2..1000000], isPrime x, all isPrime (rotations x)]

main = print $ answer
