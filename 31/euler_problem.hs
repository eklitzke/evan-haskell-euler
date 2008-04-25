module Main where

coins = [1, 2, 5, 10, 20, 50, 100, 200]

p :: Int -> Int
p _ = 1

buildUp :: [Int] -> [Int -> Int] -> [Int -> Int]
buildUp []     fs = fs
buildUp (c:cs) (f:fs) = buildUp cs (f':f:fs)
    where
    f' a = if a >= 0 then f' (a - c) + f a else 0

main = print $ (head (buildUp (tail coins) [p])) 200
