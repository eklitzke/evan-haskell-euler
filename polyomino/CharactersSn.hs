-- CharactersSn.hs
-- Copyright (c) David Amos, 2006

import List (sort)
import QQ
import MPoly hiding (s)
import PolyaCounting (cycleIndexSn)
import CombinatoricsCounting (factorial)


-- SYMMETRIC POLYNOMIALS

-- elementary symmetric poly in m variables over partition lambda
e _ [0] = 1
e m [d] | m >= d    = e (m-1) [d] + x_ m * e (m-1) [d-1]
        | otherwise = 0
e m lambda = product [e m [l] | l <- lambda]

-- complete symmetric poly in m variables over partition lambda
h _ [0] = 1
h 1 [d] = x1 ^ d
h m [d] | m > 1 = sum [x_ m ^ i * h (m-1) [d-i] | i <- [0..d]]
h m lambda = product [h m [l] | l <- lambda]

-- power sum symmetric poly in m variables over partition lambda
p m [d] = sum [x_ i ^ d | i <- [1..m]]
p m lambda = product [p m [l] | l <- lambda]


-- THE SCHUR POLYNOMIAL FROM YOUNG TABLEAUX

rows m (x:xs) = [y:ys | y <- [x..m], ys <- rows m (zipWith max (repeat y) xs)]
rows m [] = [[]]

successors m (l:ls,r:rs) = let start = map (+1) (take l r) in [(ls,r':r:rs) | r' <- rows m start]
successors m (l:ls,[]) = let start = replicate l 1 in [(ls,[r]) | r <- rows m start]
successors m ([],_) = []

isTerminal ([],_) = True
isTerminal _ = False

-- depth first tree search
dfts (isTerminal, successors) initial = dfts' initial
    where dfts' node | isTerminal node = node : more
                     | otherwise       = more
                     where more = concatMap dfts' (successors node)

allTableaux m lambda = map (reverse . snd) (dfts (isTerminal, successors m) (lambda,[]))

-- Schur polynomial
s m lambda = sum [monomialFromTableau t | t <- allTableaux m lambda]
    where monomialFromTableau t = product [x_ i | i <- concat t]


-- EXPRESSING SCHUR POLYNOMIAL IN TERMS OF OTHER SYMMETRIC POLYNOMIALS

altSum xs = foldr (-) 0 xs

remove i zs = let (xs,y:ys) = splitAt i zs in xs ++ ys

det [[x]] = x
det (row:rows) = let minors = [map (remove i) rows | i <- [0..length row - 1]]
                 in altSum (zipWith (*) row (map det minors))

fMatrix f n = [[f i j | j <- [1..n]] | i <- [1..n]] 
-- duplicated from matrix.hs

-- The Schur polynomial in terms of the complete symmetric polynomials
s_h lambda = det (fMatrix f (length lambda))
    where f i j = let k = lambda !! (i-1) - i + j
                  in case compare k 0 of
                     LT -> 0
                     EQ -> 1
                     GT -> x_ k

-- The Schur polynomial in terms of the power sum polynomials
s_p lambda = substMP (s_h lambda) (1:[cycleIndexSn n | n <- [1..]])

s_x m lambda = substMP (s_p lambda) [p m [i] | i <- [0..]]

toPartition (c,as) = (concat [replicate a i | (i,a) <- zip [(0::Int)..] as], c)

characterSn n lambda | n == sum lambda =
    let classTerms = map toPartition $ termsMP $ s_p lambda
        classSizes = map toPartition $ termsMP $ cycleIndexSn n
    in [(classTerms `lookup` partition) / size | (partition,size) <- sort classSizes]
    where lookup ((q,c):qs) p | p == q = c
                              | otherwise = lookup qs p
          lookup [] _ = 0

characterTableSn n =
    let (classes,sizes) = unzip $ sort $ map toPartition $ termsMP $ cycleIndexSn n
        partitions = reverse $ sort $ map reverse classes
        characters = [(lambda, characterSn n lambda) | lambda <- partitions]
    in (classes, map (m*) sizes, characters)
    where m = fromInteger (factorial n)

innerProduct weights u v = sum [weight*x*y | (weight,x,y) <- zip3 weights u v] / sum weights :: QQ
