-- six.hs

-- inspired by http://math.ucr.edu/home/baez/six.html

import List (intersect, sort)
import PermutationGroups
import PolyaCounting (cycleIndexSn)
import CombinatoricsCounting (factorial, choose)

disjoint xs ys = xs `intersect` ys == []

-- 15 pairs of points from [1..6]
duads = [[i,j] | i <- [1..6], j <- [i+1..6]] :: [[Int]]

-- 15 different ways to pick three disjoint duads from [1..6]
synthemes = [ [d1,d2,d3] | d1 <- duads,
                           d2 <- duads, d2 > d1, disjoint d1 d2,
                           d3 <- duads, d3 > d2, disjoint (d1++d2) d3 ]

-- 6 different ways to partition the 15 duads into 5 synthemes
pentads = [ [s1,s2,s3,s4,s5] | s1 <- synthemes,
                               s2 <- synthemes, s2 > s1, disjoint s1 s2,
                               s3 <- synthemes, s3 > s2, disjoint (s1++s2) s3,
                               s4 <- synthemes, s4 > s3, disjoint (s1++s2++s3) s4,
                               s5 <- synthemes, s5 > s4, disjoint (s1++s2++s3++s4) s5 ]

-- induced actions of an elt of S6 on duads, synthemes and pentads
duad -^ g = sort [pt .^ g | pt <- duad]
syntheme +^ g = sort [duad -^ g | duad <- syntheme]
pentad *^ g = sort [syntheme +^ g | syntheme <- pentad]

-- A numbering allowing us to treat the six pentads as six points
numbering = zip pentads ([1..6] :: [Int])

-- The outer automorphism of S6 induced by the action of S6 on the pentads
outer g = PL [let Just p'= lookup (p *^ g) numbering in p' | p <- pentads]


takeCycle (x:xs) = x : takeWhile (/= x) xs


-- EXPERIMENTS
-- Generalizing the construction to other Sn

duads' n = [[i,j] | i <- [1..n], j <- [i+1..n]] :: [[Int]]

synthesize _ rs 0 = [reverse rs]
synthesize [] _ _ = []
synthesize (l:ls) rs i =
    (if disjoint l (concat rs) then synthesize ls (l:rs) (i-1) else [])
    ++ synthesize ls rs i

synthemes' n | even n = synthesize (duads' n) [] (n `div` 2)

pentads' n = synthesize (synthemes' n) [] (n-1) -- (length (duads' n) `div` (n `div` 2))


countAloud xs = map fst (zip [1..] xs)

main = print (map (\n -> (n, length (pentads' n))) [6,8..])