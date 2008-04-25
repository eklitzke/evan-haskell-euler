module Main where
import EulerMath
import Data.List

answer = (sort (permutations "0123456789")) !! 999999
main = putStrLn answer
