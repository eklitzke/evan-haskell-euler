module Main where

import Data.Array.IO

maxNum :: Int
maxNum = 10^6

collatz :: Int -> Int
collatz n = if even n then n `div` 2 else 3 * n + 1

doCollatz :: Int -> IOArray Int Int -> IO Int
doCollatz n cache = if n >= maxNum
                      then doCollatz (collatz n) cache
                      else do c <- readArray cache n
                              if c > 0
                                then return c
                                else do r <- doCollatz (collatz n) cache
                                        writeArray cache n (1 + r)
                                        return (1 + r)

main :: IO ()
main = do arr <- newArray (1, maxNum) 0 :: IO (IOArray Int Int)
          writeArray arr 1 1
          mapM (\n -> doCollatz n arr) [1..maxNum-1]
          readArray arr 100 >>= print
