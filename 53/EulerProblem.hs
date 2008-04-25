module Main where

import EulerMath
import Data.List

main = print $ length $ filter (> 1000000) [nCr n r | n <- [1..100], r <- [1..n]]
