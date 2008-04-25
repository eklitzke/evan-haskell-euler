module Main where

odds :: [Int]
odds = take 500 (filter odd [3..])

f :: Int -> Int -> Int
f n x = x*x - n*x + n

answer :: Int
answer = 1 + (sum $ concat [map (f n) odds | n <- [0..3]])

main :: IO ()
main = print answer
