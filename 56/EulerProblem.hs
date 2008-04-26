module Main where

import Data.Char

digitalSum :: Integral a => a -> Int
digitalSum = sum . map digitToInt . show

powers :: Integral a => [a]
powers = [a^b | a <- [1..99], b <- [1..99]]

main = print $ maximum $ map digitalSum powers
