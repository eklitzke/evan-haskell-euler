module Main where

import Data.List
import EulerMath

pandigital :: String -> Bool
pandigital s = (length s == 9) && (sort s == "123456789")

sat :: Int -> Int -> Bool
sat a b = pandigital (concat (map show [a, b, a*b]))

answer = sum $ nub [a * b | a <- [1..999], b <- [a..9999], sat a b]

main = print answer
