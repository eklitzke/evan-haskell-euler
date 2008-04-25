-- ideals.hs

module Ideals ( (<==>), memberIdeal, memberGB,
				eliminatev,
				idealSum, idealProduct, idealIntersection, lcmMP,
				polyQuotient, idealQuotient, polySaturation, idealSaturation,
				monomialLcm, monomialGcd, monomialDiv, reducedMonomialBasis, monomialPolyQuotient
				) where

import QQ
import FF
import MPoly
import GBasis
import MathsPrimitives ( ($.) )

-- Most of these algorithms are explained in
-- Cox et al, Ideals, Varieties and Algorithms (2nd edition)


-- EQUALITY OF IDEALS

fs <==> gs = gb fs == gb gs


-- IDEAL MEMBERSHIP

memberGB f gs = r == 0
	where (_,r) = quotRemMP f gs

memberIdeal f gs = memberGB f (gb gs)


-- ELIMINATION
-- Two main uses
-- 1. Implicitization
-- Given a curve defined parametrically, eg x = (1-t^2)/(1+t^2), y = 2t/(1+t^2
-- Can we eliminate t to find an expression for the curve in just x and y
-- 2. Envelopes (but not sure what they're for yet)


eliminate l fs =
	let
		fs' = map (changeTermOrder (Elim l)) fs
		gs = gb fs'
		gs' = filter (\g -> all (==0) (take l (lp g))) gs
	in map (changeTermOrder Grevlex) gs'

eliminatev vs fs =
	let
		(_,vs') = head (termsMP (product vs))    -- ie if we pass in [x,y], we'll get [..1,1..].
		fs' = map (changeTermOrder (Elimv vs')) fs
		gs = gb fs'
		gs' = filter (\g -> vs' $. lp g == 0) gs
	in map (changeTermOrder Grevlex) gs'



-- IDEAL SUM
-- Geometric interpretation: V(I+J) = V(I) `intersect` V(J)

-- Cox et al, p181
idealSum fs gs = fs ++ gs


-- IDEAL PRODUCT
-- Geometric interpretation: V(I.J) = V(I) `union` V(J)

-- Cox et al, p183
idealProduct fs gs = [f * g | f <- fs, g <- gs]


-- IDEAL INTERSECTION
-- Geometric interpretation: V(I^J) = V(I) `union` V(J)
-- (Intersection is more natural than product, because it preserves radicals)
-- Cox et al, p185-6

-- !! Can we rewrite this so that it doesn't break encapsulation of MPoly
idealIntersection fs gs =
	let
		hs =                                                                                          -- introduce a new variable t
			map (\(MP params f) -> MP params (map (\(c,as) -> (c,1:as)) f)) fs ++                                 -- f -> tf
			map (\(MP params g) -> MP params (map (\(c,as) -> (c,0:as)) g ++ map (\(c,as) -> (-c,1:as)) g)) gs    -- g -> (1-t)g
		hs' = eliminate 1 hs                                                       -- now eliminate t
		hs'' = map (\(MP params h) -> MP params (map (\(c,as) -> (c, drop 1 as)) h)) hs'                  -- remove t from the ring
	in hs''

-- Cox et al, p187
lcmMP f g = h where [h] = idealIntersection [f] [g]

-- See also http://en.wikipedia.org/wiki/Gr%C3%B6bner_basis


-- IDEAL QUOTIENT
-- Geometric interpretation: V(I:J) >= Zariski closure of V(I) - V(J)
-- (with equality if k algebraically closed and I radical)

-- Cox et al, p193-4

polyQuotient fs g =
	let hs = idealIntersection fs [g]
	in map (\h -> h // g) hs
	where
		h // g = let ([u],_) = quotRemMP h [g] in u

idealQuotient _I fs =
	let _Ifis = map (polyQuotient _I) fs
	in foldl1 idealIntersection _Ifis



-- IDEAL SATURATION (BY A POLY)
-- Cox et al p195 (exercise 9)
-- (Also from Stillman paper)

-- !! Can we rewrite this so that it doesn't break encapsulation of MPoly
polySaturation fs (MP params g) =
	let
		fs' = map (\(MP params f) -> MP params (map (\(c,as) -> (c,0:as)) f)) fs                   -- add new variable t
		g' = MP params ((-1,[]) : map (\(c,as) -> (c,1:as)) g)                                    -- tg-1 (we don't worry about the term order as eliminate will fix that)
		hs = eliminate 1 (g' : fs')
		hs' = map (\(MP params h) -> MP params (map (\(c,as) -> (c, drop 1 as)) h)) hs
	in hs'
-- agrees with Macaulay2 on saturate (ideal(y^3*x^2+2*y^2*x+3*x*y, 3*y^2+x*y-3*y), y)
-- == [y^3*x^2+2*y^2*x+3*x*y, 3*y^2+x*y-3*y] `polySaturation` y
-- Macaulay2 book 103-4 (although they work over F7)

-- !! Not sure if this is correct.
-- [x^5+y^3+z^3, x^3+y^5+z^3, x^3+y^3+z^5] `idealSaturation` [x,y,z] doesn't agree with Macaulay2 book p62, answer for saturate(ideal(x^5+y^3+z^3, x^3+y^5+z^3, x^3+y^3+z^5))
-- (which I think means saturate wrt <x,y,z>)
idealSaturation _I fs =
	let _Ifis = map (polySaturation _I) fs
	in foldl1 idealIntersection _Ifis




-- MONOMIAL IDEALS

-- Some of the above operations on ideals are easier if we know that all the polynomials involved are monomials

monomialLcm (MP params [t]) (MP _ [u]) = MP params [lcmTm t u]

monomialGcd (MP params [t]) (MP _ [u]) = MP params [gcdTm t u]

monomialDiv (MP params [t]) (MP _ [u]) = MP params [divTm t u]

reducedMonomialBasis gs = doReduce [] gs
	where
		doReduce gs' [] = gs'
		doReduce gs' (g:gs) =
			if any (`dividesTm` (lt g)) [lt h | h <- gs' ++ gs]
			then doReduce gs' gs
			else doReduce (g:gs') gs

monomialPolyQuotient ms f = reducedMonomialBasis [m `monomialDiv` (m `monomialGcd` f) | m <- ms]





-- PRIMARY DECOMPOSITION
-- To do this, we need to be able to factor multivariate polynomials

-- If J = intersection Qi is a primary decomposition, each Qi is Pi-primary
-- then J : f = intersection Qj, where the Qj are only those primaries s.t f not a member of Pj
 
-- geometric interpretation
-- components of V (J : f^inf) are precisely the components of V(J) which do not lie on the hypersurface f = 0

