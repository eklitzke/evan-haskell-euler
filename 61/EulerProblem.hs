module Main where

import Data.Set as S
import Data.List as L

fours :: [Int]
fours = [1000 .. 9999]

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

firstAndLast xs = (take 2 $ head xs) == (drop 2 $ last xs)

cyclic (x:y:xs) = ((drop 2 x) == (take 2 y)) && (cyclic (y:xs))
cyclic _        = True

-- Tests if a list of numbers is cyclic, as defined in the problem
hasCyclicProperty :: [Int] -> Bool
hasCyclicProperty = hasCyclicProperty' . L.map show
    where
    hasCyclicProperty' :: [String] -> Bool
    hasCyclicProperty' xs = (cyclic xs) && (firstAndLast xs)

hasPolygonalProperty :: [Int] -> Bool
hasPolygonalProperty [a,b,c,d,e,f] = and [ a `S.member` triangles
                                         , b `S.member` squares
                                         , c `S.member` pentagonals
                                         , d `S.member` hexagonals
                                         , e `S.member` heptagonals
                                         , f `S.member` octagonals ]

-- Allowable two digit sequences
allowable :: [Int]
allowable = [x | x <- [11..99], (x `mod` 10) /= 0]

generateNextCyclic :: Int -> [Int]
generateNextCyclic x =
    let lastTwo = (read (drop 2 $ show x)) * 100
     in [lastTwo + x | x <- allowable]


possibleStarts :: [Int] 
possibleStarts = [a * 100 + b | a <- allowable | b <- allowable]

generateCyclics :: Int -> [[Int]]
generateCyclics n = generate' 5 n
    where
    generate' 0 _ = []
    generate' n x = map ((++) (map (n:) (generateNextCyclic n))

candidates = [[

map read [
    let beg = read $ take 2 $ show x
     in
         
    
    | (read $ take 2 $ show x) < 10 = []
    | otherwise                     = 
generateNextCyclic x =
    where


hasBoth f g = \x -> (f x) && (g x)

myList = [[a,b,c,d,e,f] | a <- [1000..9999], b <- [a..9999], c <- [b..9999], d <- [c..9999], e <- [d..9999], f <- [e..9999]]

main = print $ L.filter (hasBoth hasPolygonalProperty hasCyclicProperty) myList
{-

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
    -}
