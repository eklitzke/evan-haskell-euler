module Main where

import Data.List

answer = length $ nub [a^b | a <- [2..100], b <- [2..100]]
main = putStrLn $ show answer
