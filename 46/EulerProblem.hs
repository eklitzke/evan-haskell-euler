module Main where

import Primes

primes :: [Int]
primes = filter isPrime [2..]

dsquares :: [Int]
dsquares =  [2*(x*x) | x <- [1..]]

oddComposite :: [Int]
oddComposite = [x | x <- [3..], odd x, not (isPrime x)]

myHead :: [a] -> [a]
myHead [] = []
myHead xs = [head xs]

goldbachSolution :: Int -> [(Int, Int)]
goldbachSolution n = myHead [(p, d) | p <- ps, d <- ds, (p + d) == n]
    where
    ps = takeWhile (< n) primes
    ds = takeWhile (< n) dsquares

main = print $ head [n | n <- oddComposite, goldbachSolution n == []]
