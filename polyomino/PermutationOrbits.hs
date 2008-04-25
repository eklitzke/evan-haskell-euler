-- permutationorbits.hs

module PermutationOrbits where

-- orbits of points, pairs, and subsets under permutation actions

import PermutationGroups
import MergeSort
import RedBlackTree
import List (delete, intersect)


class Permutable a where
	action :: Permutation -> a -> a

instance Permutable Int where
	action g x = x .^ g


----------------------------

newtype Edge = E (Int,Int) deriving (Eq, Ord, Show)
-- it's a bit of a pain to have to define this as a newtype
-- the problem is that GHC won't accept type aliases, or compound types, in instance declarations. (Hugs will)

type Graph = [Edge]

toEdge edge@(i,j) = if i < j then E edge else E (j,i)

toGraph edges = map toEdge edges

instance Permutable Edge where
	action permutation (E (i,j)) = toEdge (action permutation i, action permutation j)

isGraphAutomorphism edges permutation = mergeSort edges == mergeSort (map (action permutation) edges) 
-- an automorphism of a graph is a permutation of the nodes that takes edges to edges



newtype Block = B [Int] deriving (Eq, Ord, Show)

type SetSystem = [Block]

toBlock ps = B (mergeSort ps)

toSetSystem blocks = map toBlock blocks

instance Permutable Block where
	action permutation (B ps) = toBlock (map (action permutation) ps)

isSetSystemAutomorphism blocks permutation = mergeSort blocks == mergeSort (map (action permutation) blocks)
-- an automorphism of a set system is a permutation of the points that takes blocks to blocks


-- the orbit of a point, edge or block under the action of a set of permutations
orbit :: (Ord a, Permutable a) => [Permutation] -> a -> [a]
orbit gs x = doOrbit (rbfromlist [x], [x])
	where
		doOrbit (ps, lastps) =
			let newps = filter (\p -> not (p `rbmember` ps)) [action g p | g <- gs, p <- lastps]
			in
				if null newps
				then rbtolist ps
				else doOrbit (foldl rbinsert ps newps, newps)

isAutomorphism :: (Ord a, Permutable a) => Permutation -> [a] -> Bool
isAutomorphism permutation ps = mergeSort ps == mergeSort (map (action permutation) ps)

isIsomorphism g a b = mergeSort (map (action g) a) == mergeSort b


-- TRANSITIVITY

-- work out whether the group is transitive from generators
isTransitive :: Int -> [Permutation] -> Bool
isTransitive n gs = length (orbit gs (1::Int)) == n

-- work out whether the group is transitive from Schreier-Sims representation
isTransitiveSS n (_,t:_) = length (rbtolist t) == n 


-- PRIMITIVITY
-- A subset Y of X is called a block if, for all g <- G, either Yg == Y, or Yg `intersect` Y == []
-- A group is called primitive if it has no non-trivial blocks
-- (The trivial blocks are the one-element subsets and the whole set X)

-- The following functions are inefficient - they consider every elt of the group
-- Need to code the more efficient algorithms from Seress
-- For the following functions, you need to pass in the whole group, not just the generators

isBlock :: [Permutation] -> [Int] -> Bool
isBlock gs xs = all (`elem` [0,m]) [length (xs  `intersect` xs') | g <- gs, xs' <- [[action g x | x <- xs]] ]
	where m = length xs
-- perhaps define another instance of Permutable, which is an *unordered* list of Ints (like Block, but without the mergeSort)

k_subsets 0 xs = [[]]
k_subsets _ [] = []
k_subsets k (x:xs) = map (x:) (k_subsets (k-1) xs) ++ k_subsets k xs

nonTrivialBlocks n gs = [xs | k <- [2..n-1], xs <- k_subsets k [1..n], isBlock gs xs]

isPrimitive n gs = null (nonTrivialBlocks n gs)


-- the following function assumes that we have a set of Schreier-Sims transversals
-- this isn't actually required for the algorithm (we only use the first set of transversals)
-- loosely based on Seress p101
findBlockOfImprimitivity n (a:_,_:s:_,t:_) = -- we ought to check the group is transitive first
	let
		bs = [1..a-1] ++ [a+1..n]
		rbs = [rb | b <- bs, Just rb <- [t `rblookup` b] ]    -- ie rb is the element of g such that a^rb = b (exists because the group is transitive)
		blocks = [orbit (rb:s) a | rb <- rbs]      -- the orbit of a in <Ga,rb>
	in
		blocks
-- need to think more about this
-- some of the blocks come out trivial [1..n], and some not
-- I think this is analogous to the way that some elements of a group generate the whole group, and some not
-- (where the group in question would be the rbs acting on the cosets of Ga within G)

