module Main where

import Data.Set as S
import Data.List

makePolygonalSet :: (Int -> Int) -> Set Int
makePolygonalSet f = fromDistinctAscList $ fourDigits [f n | n <- [1..]]
    where fourDigits = takeWhile (<= 9999) . dropWhile (< 1000)

triangles, squares, pentagonals, hexagonals, heptagonals, octagonals :: Set Int
triangles   = makePolygonalSet (\n -> (n*(n+1)) `div` 2)
squares     = makePolygonalSet (\n -> n*n)
pentagonals = makePolygonalSet (\n -> (n*(3*n-1)) `div` 2)
hexagonals  = makePolygonalSet (\n -> n * (2*n - 1))
heptagonals = makePolygonalSet (\n -> (n*(5*n-3)) `div` 2)
octagonals  = makePolygonalSet (\n -> n * (3*n - 2))

hasCyclicProperty :: [Int] -> Bool
hasCyclicProperty ys = hasCyclicProperty' ys
    where
    firstTwo = take 2 . show
    lastTwo = drop 2 . show
    hasCyclicProperty' [x] = lastTwo x == firstTwo (head ys)
    hasCyclicProperty' (x:x':xs) = (lastTwo x == firstTwo x') && (hasCyclicProperty' (x':xs))

allSets = triangles `S.union` squares `S.union` pentagonals `S.union` hexagonals `S.union` heptagonals `S.union` octagonals

makeNPerms :: Int -> [a] -> [[a]]
makeNPerms 0 _  = [[]]
makeNPerms n xs = [x:y | x <- xs, y <- makeNPerms (n-1) (tail xs)]

-- means there is an ordering of the list that is cylic
--cyclicLists :: [a] -> [[a]]
--cyclicLists = filter hasCyclicProperty $ makeNPerms 6 allSets

prefixes :: Set String
prefixes = S.map (take 2 . show) allSets

postfixes :: Set String
postfixes = S.map (drop 2 . show) allSets

pruneSet :: Set Int -> Set Int
pruneSet = S.intersection pruned
    where
    pruned = S.filter (\x -> let sx = show x in ((take 2 sx) `member` prefixes) && (drop 2 sx) `member` postfixes) allSets

pt = pruneSet triangles
ps = pruneSet squares
pp = pruneSet pentagonals
px = pruneSet hexagonals
ph = pruneSet heptagonals
po = pruneSet octagonals

ptl = toAscList pt
psl = toAscList ps
ppl = toAscList pp
pxl = toAscList px
phl = toAscList ph
pol = toAscList po

main = print $ product [S.size s | s <- [pt, ps, pp, px, ph, po]]

--main = print $ (length $ nub [take 2 $ show x| x <- allSets], length $ nub [drop 2 $ show x| x <- allSets])
{- main = do
    print $ S.size pt
    print $ S.size ps
    print $ S.size pp
    print $ S.size px
    print $ S.size ph
    print $ S.size po
    -}
