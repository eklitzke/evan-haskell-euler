module Main where

import EulerMath
import Data.Char
import Data.List
import Primes

pandigitalsN :: (Read a, Integral a) => Int -> [a]
pandigitalsN n = map read pperms
    where
    pperms = permutations ['1' .. intToDigit n]

rcmp a b = compare b a

pans = sortBy rcmp (concat [pandigitalsN n | n <- [7..9]])

main = print (head [p | p <- pans, isPrime p])
