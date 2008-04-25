module Main where

import Primes
import EulerMath
import Data.Char
import Data.List

replaceAt :: [a] -> a -> Int -> [a]
replaceAt xs c n = (take n xs) ++ (c : (drop (n + 1) xs))

replacePositions :: [a] -> a -> [Int] -> [a]
replacePositions target _ [] = target
replacePositions target c (p:ps) = replacePositions (replaceAt target c p) c ps

intFamily :: String -> Char -> [Int] -> [Int]
intFamily s c p = read $ replacePositions s c p

primeFamilyLen :: String -> [Int] -> Int
primeFamilyLen s p = length $ nub $ filter (isPrime . read) [rp | c <- ['0'..'9'], let rp = replacePositions s c p, head rp /= '0']

longestPFL :: Int -> Int
longestPFL n = maximum [primeFamilyLen sn p | p <- ps, p /= []]
    where
    sn = show n
    ln = length sn
    ps = powerset [0..ln-1]

main = print $ head $ tail [x | x <- [56003..], isPrime x, longestPFL x == 8]
