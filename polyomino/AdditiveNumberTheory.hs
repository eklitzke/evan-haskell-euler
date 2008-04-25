-- AdditiveNumberTheory.hs

module AdditiveNumberTheory where

import Primes (isPrime)
import PowerSeries

-- setsums k xs n tries to write n as a sum of up to k elts from xs
-- for efficiency, xs should be ordered largest first
setsums _ _ 0 = [[]]
setsums 0 _ _ = []
setsums _ [] _ = []
setsums k (x:xs) n | x > n     = setsums k xs n
                   | otherwise = map (x:) (setsums (k-1) (x:xs) (n-x)) ++ setsums k xs n

squares = [n*n | n <- [1..] ] :: [Int]

squaresums k n = setsums k (reverse $ takeWhile (<= n) squares) n


cubes = [n*n*n | n <- [1..] ] :: [Int]

cubesums k n = setsums k (reverse $ takeWhile (<= n) cubes) n


triangles = scanl1 (+) [1..] :: [Int]

squares' = scanl1 (+) [1,3..] :: [Int]

pentagons = scanl1 (+) [1,4..] :: [Int]


-- USING POWER SERIES

-- squarePS = PS (1 : [if isSquare n then 1 else 0 | n <- [1..]])

squarePS = diagonalSumPS [t^n^2 | n <- [0..] ]

-- cubePS = PS (1 : [if isCube n then 1 else 0 | n <- [1..]])

cubePS = diagonalSumPS [t^n^3 | n <- [0..] ]


-- SIEVES

newtype Sieve = S [[[Int]]] deriving (Eq,Show)

terms (S ts) = ts

-- addSieve superimposes two sieves (so it's like OR)
addSieve (t:ts) (u:us) = merge t u : addSieve ts us
addSieve ts [] = ts
addSieve [] us = us

-- merge takes two sorted sets of "vectors", and merges them, removing duplicates
merge (v:vs) (w:ws) = case compare v w of
                      GT -> v : merge vs (w:ws)
                      LT -> w : merge (v:vs) ws
                      EQ -> v : merge vs ws
merge vs [] = vs
merge [] ws = ws

-- multSieve places a copy of one sieve at each point of another sieve
multSieve (t:ts) (u:us) = [t `combine` u] `addSieve`
                          ( [] : map (t `combine`) us ) `addSieve`
                          ( [] : map (`combine` u) ts ) `addSieve`
                          ([] : [] : multSieve ts us)
multSieve _ _ = []

combine t u = [v ++ w | v <- t, w <- u, null v || null w || last v >= head w] 

instance Num Sieve where
    S ts + S us = S (addSieve ts us)
    S ts * S us = S (multSieve ts us)


isSquare n = n `elem` takeWhile (<= n) squares

squareSieve = S ([[]] : [ if isSquare n then [ [n] ] else [] | n <- [1..] ])

fourSquaresSieve = squareSieve ^ 4

isCube n = n `elem` takeWhile (<= n) cubes

cubeSieve = S ([[]] : [ if isCube n then [ [n] ] else [] | n <- [1..] ])


triangleSieve = S ([[]] : [ if isTriangle n then [ [n] ] else [] | n <- [1..] ])
    where isTriangle n = n `elem` takeWhile (<= n) triangles

pentagonSieve = S ([[]] : [ if isPentagon n then [ [n] ] else [] | n <- [1..] ])
    where isPentagon n = n `elem` takeWhile (<= n) pentagons


primeSieve = S ([[]] : [ if isPrime $ toInteger n then [ [n] ] else [] | n <- [1..] ])

oddElts (x0:x1:xs) = x1 : oddElts xs
oddElts _ = []

evenElts (x:xs) = x : oddElts xs
evenElts [] = []
