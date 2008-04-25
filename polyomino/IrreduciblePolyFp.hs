-- irreduciblepolyfp.hs

module IrreduciblePolyFp where

import FF
import UPoly
import NumberTheoryFundamentals (power)
import RandomGenerator (randomInteger)



charUPFF (UP (F p _:_)) = p


-- irreducible polynomials

ff2_2 = UP (map (F 2) [1,1,1])
ff2_3 = UP (map (F 2) [1,1,0,1])
ff2_4 = UP (map (F 2) [1,1,0,0,1])
ff2_6 = UP (map (F 2) [1,1,0,0,0,0,1])
ff2_10 = UP (map (F 2) [1,0,0,1,0,0,0,0,0,0,1])

ff5_2 = UP (map (F 5) [2,0,1])
ff5_4 = UP (map (F 5) [2,0,0,0,1])
ff5_8 = UP (map (F 5) [2,0,0,0,0,0,0,0,1])
ff5_16 = UP (map (F 5) [2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1])


-- should put code here to test whether a poly is irred, and to find irreds

fq p d = findIrreduciblePolyFp p d
-- will eventually use sparse irreducible polys where available


randomMonicPolysFp p d =
	let coeffs = map fst (iterate (\(_,seed) -> randomInteger p seed) (0,2386287367))
	in polys coeffs
	where polys coeffs =
		let (as, coeffs') = splitAt d coeffs
		in UP (map (F p) (as ++ [1])) : polys coeffs'



-- FINDING IRREDUCIBLE POLYNOMIALS OVER F_P

-- This section uses results from Gao, Panario, "Tests and Constructions of Irreducible Polynomials over Finite Fields"

-- The first part of the paper discusses ways of testing whether a given polynomial is irreducible over Fp
-- The second part shows how to construct some infinite families of sparse irreducible polynomials over Fp

-- We will prefer to construct a polynomial from an infinite family
-- If none is available however, then we construct random polys and test them for irreducibility

-- Ben-Or irreducibility test - see Gao et al.

isIrreduciblePolyFp f =
	let
		d = degUP f
		p = charUPFF f
		polys_x_p_i = tail (iterate (\g -> power (UP [1%p], \h1 h2 -> h1 * h2 `modUP` f) g p) (UP [0%p,1%p]))       -- == [x^p^i (mod f) | i <- [1..]]
		testPolys = map (UP [0%p, (p-1)%p] +) polys_x_p_i          -- == [x^p^i - x | i<- [1..]]
		testResults = map (\g -> degUP (gcdUP f g) == 0) testPolys
	in and (take (d `div` 2) testResults)

findIrreduciblePolyFp p d = head [f | f <- randomMonicPolysFp p d, isIrreduciblePolyFp f]


-- TODO

-- Sparse irreducible polys (eg binomials, which when they exist are trivial to construct)