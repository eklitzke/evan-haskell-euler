module Main where

import Primes
import EulerMath
import Data.List

intLength :: Int -> Int
intLength = length . show

-- tests that they are all prime and permutations of each other
magic :: [Int] -> Bool
magic (x:xs) = (all (== h) t) && (all isPrime (x:xs))
    where
    ss = sort . show
    h = ss x
    t = map ss xs

answer :: String
answer = concat $ map show $ head $ tail [s | x <- [1000..9997], y <- [1..9000], let s = [x, x + y, x + y + y], intLength (last s) == 4,  magic s]

main = putStrLn answer
