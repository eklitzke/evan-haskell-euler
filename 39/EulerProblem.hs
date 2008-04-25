module Main where

import Data.List

validTri :: Int -> Int -> Int -> (Int, Int, Int)
validTri a b c = if ineq && pyth then (a', b', c') else (0, 0, 0)
    where
    abc = [a, b, c]
    a'  = minimum [a, b, c]
    c'  = maximum [a, b, c]
    b'  = (a + b + c) - (a' + c')
    ineq = (a' + b') > c'
    pyth = (a'^2 + b'^2) == c'^2

numForPerim :: Int -> Int
numForPerim p = length $ nub [t | a <- [1..p-2], b <- [a..p-1], let c = p - (a + b), let t = validTri a b c, t /= (0, 0, 0)]

perims = [(p, numForPerim p) | p <- [3..999]]
myCmp (a, b) (c, d) = compare b d

answer = fst $ maximumBy myCmp perims

main = print answer
