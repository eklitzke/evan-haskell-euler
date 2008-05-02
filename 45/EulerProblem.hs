module Main where

import Data.IntSet hiding (map, filter)

t = [(n * (n+1)) `div` 2 | n <- [1..]]
p = [(n * (3*n-1)) `div` 2 | n <- [1..]]
h = [(n * (2*n-1)) | n <- [1..]]

ts = fromDistinctAscList (take 100000 t)
ps = fromDistinctAscList (take 100000 p)
hs = fromDistinctAscList (take 100000 h)

main = print $ last $ toAscList $ ts `intersection` ps `intersection` hs
