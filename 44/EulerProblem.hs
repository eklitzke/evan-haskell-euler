module Main where

import Data.Set

pentagonals = [(n * (3*n -1)) `div` 2 | n <- [1..10000]]
penset = fromList pentagonals

penpairs = [abs (a - b) | a <- pentagonals, b <- pentagonals, a > b, (a - b) `member` penset, (a + b) `member` penset]
answer = minimum penpairs
main = print answer
