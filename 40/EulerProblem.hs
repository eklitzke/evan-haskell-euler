module Main where

import Data.Char

hugenum = concat (map show [1..])
nums = [hugenum!!(x-1) | x <- [1, 10, 100, 1000, 10000, 100000, 1000000]]
main = print $ product $ map digitToInt nums
