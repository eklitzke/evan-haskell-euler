module Main where

import Data.Char
import EulerMath

test :: Int -> Bool
test n = n == sum [factorial (digitToInt d) | d <- (show n)]

main = print $ sum [x | x <- [10..99999], test x]
