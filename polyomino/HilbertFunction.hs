-- hilbertfunction.hs

module HilbertFunction where

import RedBlackTree
import MathsPrimitives (($+), ($-))
import FF
import QQ
import MPoly
import GBasis
import Ideals -- for monomial ideals


-- MONOMIAL BASES

-- basis for the polynomial ring in variables vs
mbasis vs = doMbasis [1]
	where doMbasis ms = reverse ms : doMbasis (toSet [f | v <- vs, m <- ms, f <- [v*m]])

-- basis for the quotient ring R/I
mbasisQR vs ideal = doMbasisQR [1]
	where
		doMbasisQR [] = []  -- the quotient ring has a finite basis
		doMbasisQR ms = reverse ms : doMbasisQR (toSet [f | v <- vs, m <- ms, f <- [v*m], not (isReducibleMP f ideal)])


-- naive implementation of hilbert series, which simply counts basis elts of the quotient ring
hilbertSeriesQR' vs ideal = map length (mbasisQR vs ideal)


-- HILBERT INVARIANTS

-- We follow the definition in Eisenbud and Schenck, where we count the number of monomials of degree == d
-- Some authors, eg Cox et al, count the number of monomials of degree <= d instead

-- The recursive method that we use to calculate all these invariants is from Eisenbud p325 / Schenck p56
-- However, Eisenbud mentions some optimisations that we have not implemented (basically, careful choice of which monomial to recurse on)


-- HILBERT FUNCTION

-- The number of monomials in n variables of degree i is i+n-1 `choose` i (Schenck p22)
-- It's more efficient to calculate this as i+n-1 `choose` n-1, since we expect n small, i large, on average
numMonomials n i = product [i'+1..i'+n'-1] `div` product [1..n'-1]
	where (n',i') = (toInteger n, toInteger i)
-- (The reason is that you can imagine i+n-1 "x"s in a row. Then, wherever you choose an "x", it is removed,
-- but the "x"s to its right are labelled with index one higher than the "x"s to its left.
-- By n-1 such choices, you determine where each of x1..xn starts and stops, and you leave a monomial of degree i.
-- Thus it's i+n-1 `choose` n-1, which is the same as i+n-1 `choose` i
numMonomials' n i = product [n'..n'+i'-1] `div` product [1..i']
	where (n',i') = (toInteger n, toInteger i)

hilbertFnMonomialQR vs [m] i
	| i < 0     = 0
	| i < degm  = numMonomials numvars i
	| otherwise = numMonomials numvars i - numMonomials numvars (i - degm)
	where
		numvars = length vs
		degm = degMP m
hilbertFnMonomialQR vs (n:ms) i
	| i < 0     = 0
	| otherwise = hilbertFnMonomialQR vs ms i - hilbertFnMonomialQR vs (ms `monomialPolyQuotient` n) (i - degMP n)


-- Only valid if either it's a homogeneous ideal, or we using a graded term order
hilbertFnQR vs ideal i = hilbertFnMonomialQR vs (ltIdeal ideal) i


-- HILBERT SERIES

hilbertSeriesMonomialQR vs [m] = map (numMonomials numvars) [0..] $- (replicate (degMP m) 0 ++ map (numMonomials numvars) [0..])
	where numvars = length vs
hilbertSeriesMonomialQR vs (n:ms) = hilbertSeriesMonomialQR vs ms $- (replicate (degMP n) 0 ++ hilbertSeriesMonomialQR vs (ms `monomialPolyQuotient` n))


-- Only valid if either it's a homogeneous ideal, or we using a graded term order
hilbertSeriesQR vs ideal = hilbertSeriesMonomialQR vs (ltIdeal ideal)


-- HILBERT POLYNOMIAL

-- poly for number of monomials in n variables of degree x == x+n-1 `choose` x (Schenck p22)
polyNumMonomials n x = (1 / fromInteger (product [1..n'-1]), []) */ (product (map (\i->x + fromInteger i) [1..n'-1]))
	where n' = toInteger n

hilbertPolyMonomialQR vs ms = doHilbertPolyMonomialQR vs ms x
	where
		numvars = length vs
		toMPoly = fromInteger . toInteger
		doHilbertPolyMonomialQR vs [m] x = polyNumMonomials numvars x - polyNumMonomials numvars (x - toMPoly (degMP m))
		doHilbertPolyMonomialQR vs (n:ms) x = doHilbertPolyMonomialQR vs ms x - doHilbertPolyMonomialQR vs (ms `monomialPolyQuotient` n) (x - toMPoly (degMP n))

-- Only valid if either it's a homogeneous ideal, or we using a graded term order
hilbertPolyQR vs ideal = hilbertPolyMonomialQR vs (ltIdeal ideal)



-- DIMENSION ETC

dimension vs ideal = 1 + degMP (hilbertPolyQR vs ideal)




-- OBSOLETE CODE

{-
-- not as efficient as the rbtree version, but doesn't rely on Ord MPoly, which was causing problems
toSet' xs = doToSet [] xs
	where
		doToSet rs [] = rs
		doToSet rs (l:ls) = if l `elem` rs then doToSet rs ls else doToSet (l:rs) ls
-}

