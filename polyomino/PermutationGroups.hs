-- permutationgroups.hs

module PermutationGroups where

import List (sort)
import MergeSort
import RedBlackTree
import Array
import Bits (setBit, testBit)
import RandomGenerator
import CombinatoricsGeneration (permutations)


-- GROUP

class Group a where
	identity :: Int -> a
	isIdentity :: a -> Bool
	(*>) :: a -> a -> a  -- "multiply right" - multiplication, with group elements regarded as acting on the right
	inverse :: a -> a

conjugate g h = inverse h * g * h

commutator g h = inverse g * inverse h * g * h

-- prod gs = foldl1 (*>) gs
-- logically we should fold right, but as multiplication is associative we can fold left, which is more efficient
-- !! Note that we can now just use "product" instead

randomWalkOnCayleyGraph (numgs,gs) (g0,seed) = map fst (iterate doStep (g0,seed))
	where
		doStep (g,seed) =
			let (i,seed') = randomInt numgs seed
			in (g * (gs !! i), seed')
-- this is a random walk on the Cayley graph G(<gs>, gs), ie the graph with vertices the elts of <gs>, and edges (h,hg), h <- <gs> , g <- gs


-- PERMUTATION

newtype Permutation = PL [Int] deriving (Read)

-- x .^ (PL g) = g !! (x-1)
x .^ PL g = g `lookup` x
    where lookup (y:ys) 1 = y -- we count from 1, not 0
          lookup (y:ys) i = lookup ys (i-1)
          lookup [] _ = x     -- if we don't have an entry for x, then it isn't moved

-- induced action on (unordered) sets
sets =^ g = [sort [x .^ g | x <- xs] | xs <- sets]

-- induced action on (ordered) tuples
tuples <^ g = [ [x .^ g | x <- xs] | xs <- tuples]

instance Eq Permutation where
    PL xs == PL ys = equalPerm 1 xs ys
        where equalPerm i (x:xs) (y:ys) = x == y && equalPerm (i+1) xs ys
              equalPerm _ [] [] = True
              equalPerm i (x:xs) [] = i == x && equalPerm (i+1) xs []
              equalPerm i [] (y:ys) = i == y && equalPerm (i+1) [] ys

instance Ord Permutation where
    compare g@(PL xs) h@(PL ys) = if g == h then EQ else compare xs ys

instance Show Permutation where
	show g = show (toCycles' g)

instance Group Permutation where
    identity n = PL [1..n]
    isIdentity (PL xs) = and (zipWith (==) [1..] xs)
    -- PL g *> h = PL (map (.^ h) g)
    g@(PL xs) *> h@(PL ys) = PL (map (.^ h) xs') where xs' = xs ++ [length xs + 1..length ys]
    inverse (PL g) = PL (map snd (mergeSort (zip g [1..])))

instance Num Permutation where
    g@(PL xs) * h@(PL ys) = PL (map (.^ h) xs') where xs' = xs ++ [length xs + 1..length ys]
    fromInteger 1 = PL []

{-
fromCycles cs = PL (map snd (mergeSort pointActions))
	where
		pointActions = concat (map fromCycle cs)
-}

rotateL (x:xs) = xs ++ [x]

fromCycle is = zip is (rotateL is)

fromCycles cs = PL (elems (array (1,n) [(i,i) | i <- [1..n]] // (concat (map fromCycle cs))))
    where n = maximum (concat cs)

-- version which ensures you are working in Sn. This is sometimes important.
-- For example, cycleType (fromCycles' 4 [[1,2]]) is [2,1,0,0], whereas cycleType (fromCycles [[1,2]]) is [0,1]
fromCycles' n cs = PL (elems (array (1,n) [(i,i) | i <- [1..n]] // (concat (map fromCycle cs))))

toCycles (PL []) = []
toCycles (PL permutation) =
	let permutation' = rbfromlist (zip [1..] permutation)
	in findCycles ([],1,[],permutation')
	where
		findCycles (cs,i,c,mappings) =
			if rbisempty mappings
			then reverse ( map reverse (c:cs))
			else
				case mappings `rblookup` i of
				Nothing ->
					let (j,j') = rbmin mappings
					in findCycles (c:cs, j', [j], mappings `rbdelete` (j,j'))
				Just i' -> findCycles (cs, i', i:c, mappings `rbdelete` (i,i'))

toCycles' g = filter (not . isSingleton) (toCycles g)



isSingleton (x:[]) = True
isSingleton _ = False


-- CYCLE STRUCTURE

cycleType g@(PL xs) = countCycleTypes (length xs) (cyclePartition g)

-- note that the second argument must be descending ordered
countCycleTypes n xs = doCountCycleTypes n [] xs
	where
		doCountCycleTypes 0 counts _ = counts
		doCountCycleTypes i counts xs =
			let
				count = length (takeWhile (==i) xs)
				xs' = dropWhile (==i) xs
			in doCountCycleTypes (i-1) (count:counts) xs'


-- the partition of n induced by the cycle structure of g
cyclePartition g = mergeSort' (>) (map length (toCycles g))

parity g@(PL js) = 1 - 2 * ((length js - length (toCycles g)) `mod` 2)

parity' g = 1 - 2 * ((sum (map (\x -> x-1) (cyclePartition g))) `mod` 2)

orderPerm g = foldl1 lcm (cyclePartition g)


-- SOME STANDARD GROUPS

-- Sn and An, from Seress p227

generatorsSn n = map (fromCycles' n) [s,t]
	where
		s = [[1..n]]
		t = [[1,2]]

generatorsAn n = map (fromCycles' n) [s,t]
	where
		s | odd n  = [[3..n]]
		  | even n = [[1,2], [3..n]]
		t = [[1,2,3]]

-- !! Don't use SchreierSims algorithm on Sn or An generators - there are better ways (not implemented yet)

-- The cyclic group of order n
generatorsCn n = [PL (rotateL [1..n])]

-- The dihedral group on n points of order 2n
generatorsD2n n = [PL (rotateL [1..n]), PL (reverse [1..n])]



eltsSn n = map PL (permutations n)
