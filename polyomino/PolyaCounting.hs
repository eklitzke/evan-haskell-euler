-- polyacounting.hs

module PolyaCounting where

-- Cameron, Combinatorics, p247-253

import PermutationGroups
import SchreierSims
import QQ
import MPoly
import CombinatoricsCounting (factorial, choose)


-- ROTATION GROUP OF CUBE (AS PERMUTATION OF FACES)
--   1
-- 2 3 4 5
--   6

cubeF = fromCycles [[1],[2,3,4,5],[6]]
cubeV = fromCycles [[1,2,3],[4,5,6]]
cubeE = fromCycles [[1,3],[2,4],[5,6]]

cubeGp = schreierSimsTransversals 6 [cubeF, cubeV, cubeE]



-- ROTATION GROUP OF DODECAHEDRON (AS PERMUTATION OF FACES)
-- The top faces are numbered (looking from above)
--      2
--  6       3
--      1
--    5   4
-- The bottom faces are numbered so that opposite faces add up to 13 (still looking from above)
--   9    8
--     12
-- 10       7
--     11

dodecF = fromCycles [[1],[2,3,4,5,6],[11,10,9,8,7],[12]]       -- rotation about a face
dodecV = fromCycles [[1,2,3],[4,6,8],[9,7,5],[12,11,10]]       -- rotation about a vertex
dodecE = fromCycles [[1,2],[3,6],[4,9],[5,8],[7,10],[11,12]]   -- rotation about an edge

dodecGp = schreierSimsTransversals 12 [dodecF, dodecV, dodecE]
-- in fact any two generate the group

-- The order of the group is 60. It's isomorphic to A5. See:
-- http://en.wikipedia.org/wiki/Icosahedral_symmetry


cycleIndexElt g = monomial (cycleType g)

cycleIndexGp gp = sum [cycleIndexElt g | g <- fix (eltsSS gp)] / fromInteger (orderSS gp)

fix gs = let n = maximum [length xs | PL xs <- gs]
         in map (\g -> if g == PL [] then PL [1..n] else g) gs

countingPoly gp vs = substMP (cycleIndexGp gp) [sum [v^i | v <- vs] | i <- [0..]]

polyaCount' gp vs = evalMP (countingPoly gp vs) (repeat 1)

polyaCount gp vs = sum [c | (c,as) <- termsMP (countingPoly gp vs)]


-- COUNTING NON-ISOMORPHIC GRAPHS

toEdge (a,b) = if a < b then (a,b) else (b,a)

(a,b) -^ g = toEdge (a .^ g, b .^ g)    -- action on edge induced from action on points

edges :: [(Int,Int)]
edges = doEdges 2
    where doEdges n = [(i,n) | i <- [1..n-1]] ++ doEdges (n+1)
    
numEdgesKn n = fromInteger (toInteger n `choose` 2)

edgesKn n = take (numEdgesKn n) edges

edgeToPoint (a,b) = (b-2)*(b-1) `div` 2 + a

pointToEdge c = edges !! (c-1)

-- given an elt of Sn, return the induced action on the edges of Kn
inducedPermutation n g = PL [edgeToPoint (edge -^ g) | edge <- edgesKn n]


generatorsEdgeGp n = map (inducedPermutation n) (generatorsSn n)

edgeGp n = schreierSimsTransversals m (generatorsEdgeGp n)
    where m = numEdgesKn n


{-
partitions n = boundedPartitions n n
    where boundedPartitions n k    -- partitions of n where the parts are bounded to be <= k
            | n <= 0    = [[]]
            | k == 0    = []
            | k > n     = boundedPartitions n (k-1)
            | otherwise = map (k:) (boundedPartitions (n-k) k) ++ boundedPartitions n (k-1)
-}
cycleTypesSn n = doCycleTypes n n [[]]
    where doCycleTypes n 1 types = map (n:) types
          doCycleTypes n i types = concat [doCycleTypes (n-a*i) (i-1) (map (a:) types) | a <- [0..n `div` i] ]

-- the argument is [a_1, ..., a_n], where we're asking about elements with cycle structure 1^a_1 ... n^a_n
numEltsWithCycleType n as = (factorial n) `div` product [i^a * factorial a | (i,a) <- zip [1..] as]

cycleIndexSn n = sum [fromInteger (numEltsWithCycleType n as) * monomial as | as <- cycleTypesSn n] / fromInteger (factorial n)

-- given n, and a cycle type for Sn, calculate the induced cycle index on the edges of Kn
inducedCycleType :: Int -> [Int] -> [Int]
inducedCycleType n as =
    let cyclePowers = [(i,a) | (i,a) <- zip [1..] as, a /= 0]
        withinCycles = [(i, (i-1) `div` 2 * a) | (i,a) <- cyclePowers] -- each cycle induces a cycle on edges within the cycle
                    ++ [(i `div` 2,a) | (i,a) <- cyclePowers, even i]  -- but in the even case, the edge joining opposite points of the cycle has half the period
        acrossCycles = [(lcm i j, a * b * gcd i j) | (i,a) <- cyclePowers, (j,b) <- cyclePowers, i < j] -- if the points of an edge fall in cycles of different length, the period of the cycle on the edge is the gcd
                    ++ [(i, i * (a * (a-1) `div` 2)) | (i,a) <- cyclePowers, a > 1] -- when the points of an edge fall in different cycles of the same length, the cycle on the edge also has this length
			-- there are (a `choose` 2) choices for pairs of cycles, and given a pair, there are i choices for edges
    in collectPowers (withinCycles ++ acrossCycles) -- withinCycles $+ collectPowers acrossCycles
    where collectPowers powers = [sum [a | (i,a) <- powers, i == j] | j <- [1..numEdgesKn n]] -- can be done more efficiently

inducedCycleIndex n as =
    let cyclePowers = [(i,a) | (i,a) <- zip [1..] as, a /= 0]
        withinCycles = [(i, (i-1) `div` 2 * a) | (i,a) <- cyclePowers, i > 1] -- each cycle induces a cycle on edges within the cycle
                    ++ [(i `div` 2,a) | (i,a) <- cyclePowers, even i]  -- but in the even case, the edge joining opposite points of the cycle has half the period
        acrossCycles = [(lcm i j, a * b * gcd i j) | (i,a) <- cyclePowers, (j,b) <- cyclePowers, i < j] -- if the points of an edge fall in cycles of different length, the period of the cycle on the edge is the gcd
                    ++ [(i, i * (a * (a-1) `div` 2)) | (i,a) <- cyclePowers, a > 1] -- when the points of an edge fall in different cycles of the same length, the cycle on the edge also has this length
			-- there are (a `choose` 2) choices for pairs of cycles, and given a pair, there are i choices for edges
    in product [x_ i ^ a | (i,a) <- withinCycles ++ acrossCycles]


inducedCycleIndexSn n = sum [fromInteger (numEltsWithCycleType n as) * inducedCycleIndex n as | as <- cycleTypesSn n] / fromInteger (factorial n)

graphCountingPoly n = substMP (inducedCycleIndexSn n) [1+t^i | i <- [0..]]