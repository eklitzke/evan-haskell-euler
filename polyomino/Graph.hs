-- Graph.hs
-- Copyright (c) David Amos, 2006

module Graph where

import Array -- !! temporary
import RedBlackTree
import List (sort, partition, intersect, transpose, (\\) )
import MathsPrimitives (fMatrix)
import CombinatoricsGeneration (cartProd, combinationsOf, powerset, permutationsOf)
import CombinatoricsCounting (choose)
import PermutationGroups

import Primes (isPrime)
import NumberTheoryFundamentals (legendreSymbol)

-- Source
-- Godsil & Royle, Algebraic Graph Theory

set -^ g = sort [x .^ g | x <- set]

system +^ g = sort [set -^ g | set <- system]


{-
countVector _ [] = []
countVector i xs = let (ys,zs) = partition (==i) xs in length ys : countVector (i+1) zs
-}
countVector i xs = countSorted i (sort xs)

countSorted _ [] = []
countSorted i xs = let (ys,zs) = span (==i) xs in length ys : countSorted (i+1) zs

-- FAMILIES OF GRAPHS

type Graph = (Int,[[Int]])

-- cyclic graph
c n = (n, sort ([[i,i+1] | i <- [1..n-1]] ++ [[1,n]])) :: Graph

-- complete graph
k n = (n, [[i,j] | i <- [1..n-1], j <- [i+1..n]]) :: Graph
-- == subsets 2 [1..n]

-- complete bipartite graph
kb m n = (m+n, [[i,j] | i <- [1..m], j <- [m+1..m+n]]) :: Graph

-- Godsil & Royle p8
-- circulantGraph n c = [[i,j] | i <- [1..n-1], j <- [i+1..n], ((j-i) `mod` n) `elem` c]
-- !! NOT WORKING CORRECTLY


-- Generalized Johnson graph, Godsil & Royle p9
j v k i = let vertices = zip [1..] (combinationsOf k [1..v])
              edges = [ [x,y] | (x,xset) <- vertices, (y,yset) <- vertices, x < y, length (xset `intersect` yset) == i ]
          in (fromInteger (v `choose` k), edges) :: Graph

kneser v k = j v k 0

johnson v k = j v k (k-1)

petersenGraph = j 5 2 0
-- == complement $ lineGraph $ k 5

-- k-cube
q k = let vs = zip (2^k : [1..]) (powerset [1..k])
      in [ [i,j] | (i,iset) <- vs, (j,jset) <- vs, i < j, length (iset `setDiff` jset) == 1 ]
      where setDiff xs ys = (xs \\ ys) ++ (ys \\ xs)


-- actually defined for prime powers
-- (if p /= 1 mod 4, then the paley graph is a directed graph)
paleyGraph p | isPrime p' && p `mod` 4 == 1
    = (p, [ [i,j] | [i,j] <- combinationsOf 2 [1..p], legendreSymbol (j-i) p == 1 ])
    where p' = toInteger p


-- NEW GRAPHS FROM OLD

lineGraph (n,edges) = let vs = zip [1..] edges
                      in (length edges, [ [i,j] | (i,ei) <- vs, (j,ej) <- vs, i < j, ei `intersect` ej /= [] ])


complement (n,edges) = let (_,edges') = k n in (n, edges' \\ edges)


-- UTILITIES

neighbours (_,edges) i = filter (/=i) $ concat [e | e <- edges, i `elem` e]

isAdjacent (n,edges) i j = sort [i,j] `elem` edges

adjacencyMatrix graph@(n,_) = fMatrix (\i j -> if isAdjacent graph i j then 1 else 0) n

incidenceMatrix (n,edges) = [ [if v `elem` e then 1 else 0 | e <- edges] | v <- [1..n]]

valencies (n,edges) = let v = countVector 1 (concat edges) in v ++ replicate (n - length v) 0
-- the last bit is to ensure that we include vertices with valency 0

valencyVector graph = countVector 0 (valencies graph)

valencyPartition graph@(n,_) = let vvs = zip [1..] (valencies graph)
                               in [ [vertex | (vertex,valency) <- vvs, valency == i] | i <- [0..n] ]

connectedComponent graph i = cc (rbfromlist [i]) [i]
    where cc interior [] = rbtolist interior
          cc interior boundary = let boundary' = toSet [k | j <- boundary, k <- neighbours graph j, not (k `rbmember` interior)]
                                     interior' = foldl rbinsert interior boundary'
                                 in cc interior' boundary'

isConnected graph@(n,_) = connectedComponent graph 1 == [1..n]


-- AUTOMORPHISMS

eltsSymGpOf xs = [fromMapping (zip xs ys) | ys <- permutationsOf xs]
    where n = maximum xs
          fromMapping [] = PL []
          fromMapping xys = PL (elems (array (1,n) [(i,i) | i <- [1..n]] // xys))

valencyPreservingEltsSn graph = let partition = filter (not . null) (valencyPartition graph)
                               in map product $ cartProd $ map eltsSymGpOf partition

automorphisms graph@(_,edges) = [g | g <- valencyPreservingEltsSn graph, isAutomorphism2 edges g]

isAutomorphism1 edges g = edges +^ g == edges

isAutomorphism2 edges g = all (`elem` edges) [edge -^ g | edge <- edges]
-- isAutomorphism2 will spot a failure earlier, though it takes longer to verify a success


-- ISOMORPHISMS

-- prerequisites: two graphs with the same number of vertices, and the same valency vectors
-- if they don't have the same valency vectors, the result of this function won't be meaningful
valencyPreservingMap :: Graph -> Graph -> Permutation
valencyPreservingMap graph1 graph2 = let xs = concat (valencyPartition graph1)
                                         ys = concat (valencyPartition graph2)
                                     in PL (map snd $ sort $ zip xs ys)

isIsomorphism edges1 edges2 g = all (`elem` edges2) [edge -^ g | edge <- edges1]

isomorphisms1 (n,es) (m,es') | n == m    = [h | h <- eltsSn n, isIsomorphism es es' h ]
                            | otherwise = []

isomorphisms2 g1@(n1,es1) g2@(n2,es2)
    | n1 == n2 && valencyVector g1 == valencyVector g2
                = let k = valencyPreservingMap g1 g2 in [h | h <- map (k *>) (valencyPreservingEltsSn g2), isIsomorphism es1 es2 h]
    | otherwise = []

isIsomorphic g1 g2 = not $ null $ isomorphisms1 g1 g2
