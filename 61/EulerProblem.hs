module Main where

import Data.Set (Set, fromDistinctAscList, member)
import Data.List

fours :: [Int]
fours = [1000 .. 9999]

makePolygonalSet :: (Int -> Int) -> Set Int
makePolygonalSet f = fromDistinctAscList $ fourDigits [f n | n <- [1..]]
    where fourDigits = takeWhile (<= 9999) . dropWhile (< 1000)

triangles, squares, pentagonals, hexagonals, heptagonals, octagonals :: Set Int
triangles   = makePolygonalSet (\n -> (n*(n+1)) `div` 2)
squares     = makePolygonalSet (\n -> n*n)
pentagonals = makePolygonalSet (\n -> (n*(3*n - 1)) `div` 2)
hexagonals  = makePolygonalSet (\n -> n * (2 * n - 1))
heptagonals = makePolygonalSet (\n -> (n*(5*n - 3)) `div` 2)
octagonals  = makePolygonalSet (\n -> n * (3*n - 2))

-- cryptic
secret :: [a] -> [[a]]
secret []     = []
secret (x:xs) = (x:xs) : [y:x:ys | y:ys <- secret xs]

magic :: [Int] -> [Set Int] -> Bool
magic [] _ = True
magic ns (s:ss) = any magic' (secret ns)
    where magic' (x:xs) = (x `member` s) && (magic xs ss)

hasPolygonalProperty :: [Int] -> Bool
hasPolygonalProperty xs = magic xs [triangles, squares, pentagonals, hexagonals, heptagonals, octagonals]
--hasPolygonalProperty xs = magic xs [triangles, squares, pentagonals]

-- Allowable two digit sequences
allowable :: [Int]
allowable = [10..99]

nextCyclics :: Int -> [Int]
nextCyclics x = let x' = (x `mod` 100) * 100 in [x' + y | y <- allowable]

-- any cycle will have repeats generated, because the cycle can be rotated and
-- will also be a cycle. We want to avoid reprocessing these rotated cycles.
-- Therefore, we adopt that the canonical form of a cycle is the form that has
-- the smallest element as the first element.
isCanonical :: [Int] -> Bool
isCanonical (x:xs) = x < minimum xs

generateCycles :: Int -> [[Int]]
generateCycles num = filter isCanonical (generate' 6 num)
    where
    generate' 2 x = [[x,z] | z <- nextCyclics x, z `mod` 100 == t]
    generate' n x = concat [[x:y | y <- generate' (n-1) z] | z <- nextCyclics x]
    t = num `div` 100

allCycles :: [[Int]]
allCycles = concatMap generateCycles [x * 100 + y | x <- [10..99], y <- [x..99]]

main = print $ head $ filter hasPolygonalProperty allCycles
