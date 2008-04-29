module Main where

import Data.Array.IO
import Data.Array.Unboxed
import Data.List

maxNum :: Int
maxNum = 10^6

collatz :: Int -> Int
collatz n = if even n then n `div` 2 else 3 * n + 1

doCollatz :: Int -> IOUArray Int Int -> IO Int
doCollatz n cache =
    if n >= maxNum
      then do r <- (doCollatz cn cache)
              return (1 + r)
      else do c <- readArray cache n
              if c > 0
                then return c
                else do r <- doCollatz cn cache
                        writeArray cache n (1 + r)
                        return (1 + r)
    where
    cn = collatz n

fstCmp :: Ord a => (b, a) -> (c, a) -> Ordering
fstCmp (a, b) (c, d) = compare b d

main :: IO ()
main = do cache <- newArray (1, maxNum) 0 :: IO (IOUArray Int Int)
          writeArray cache 1 1
          mapM (\n -> doCollatz n cache) [1..maxNum-1]
          f <- unsafeFreeze cache :: IO (UArray Int Int)
          print $ fst $ maximumBy fstCmp (assocs f)