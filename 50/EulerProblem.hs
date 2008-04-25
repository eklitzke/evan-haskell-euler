module Main where

import Data.List
import EulerMath
import Primes

primes :: [Int]
primes = [x | x <- [2..1000000], isPrime x]

takeSmall :: Int -> [Int]
takeSmall m = takeSmallClosure 0 primes
    where
    takeSmallClosure n' (y:ys)
        | n' >= m = []
        | otherwise = y : (takeSmallClosure (n'+y) ys)

myHead :: [Int] -> Int
myHead [] = 0
myHead (x:xs) = x

conMerge :: [[a]] -> [[a]] -> [[a]]
conMerge (a:aa:as) (b:bb:bs) = a:aa:b:bb:(conMerge as bs)
conMerge [a] [b] = a:b:[]
conMerge [] [] = [[]]
conMerge [] xs = xs
conMerge xs [] = xs

consecs :: [a] -> [[a]]
consecs [] = [[]]
consecs [x] = [[x]]
consecs xs = ixs : txs : (conMerge (consecs ixs) (consecs txs))
    where
    ixs = init xs
    txs = tail xs

myConsecs :: [a] -> [[a]]
myConsecs = take 1000 . consecs

final :: Int -> [a] -> [a]
final n = reverse . take n . reverse

magic :: Int -> Int
magic p = myHead [length x | x <- cprimes, sum x == p]
    where
    ltprimes = takeSmall p
    cprimes = myConsecs ltprimes

myCmp (a, b) (c, d) = compare b d
main = print $ fst $ maximumBy myCmp $ [(p, (magic p)) | p <- (final 1000 primes)]
--main = print $ magic $ last primes
