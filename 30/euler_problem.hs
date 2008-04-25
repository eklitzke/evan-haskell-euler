module Main where

import Data.Char

fifths = sum $ take 6 [x | x <- [10..], sat x]

sat :: Int -> Bool
sat n = n == (sum $ map ((^5) . digitToInt) $ show n)

main = print fifths
