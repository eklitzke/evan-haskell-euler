-- This takes ~33 seconds on this 2.4 Ghz MBP

module Main where

import EulerMath

allPandigitals :: [String]
allPandigitals = [x | x <- (permutations "0123456789"), head x /= '0']

getSub :: Int -> Int -> [a] -> [a]
getSub start end = (take diff) . (drop start)
    where
    diff = end - start + 1

intSub :: Int -> Int -> String -> Int
intSub s e x = read $ getSub s e x

magic :: String -> Bool
magic s = all id [p1, p2, p3, p4, p5, p6, p7]
    where
    p1 = 2 `divides` intSub 1 3 s
    p2 = 3 `divides` intSub 2 4 s
    p3 = 5 `divides` intSub 3 5 s
    p4 = 7 `divides` intSub 4 6 s
    p5 = 11 `divides` intSub 5 7 s
    p6 = 13 `divides` intSub 6 8 s
    p7 = 17 `divides` intSub 7 9 s

pans = sum [read p | p <- allPandigitals, magic p]

main = print pans
