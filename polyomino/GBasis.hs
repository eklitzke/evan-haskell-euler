-- gbasis.hs

module GBasis (sPoly, gb, ltIdeal, isGBasis) where


import MergeSort
import QQ
import FF
import MPoly


-- based on "One sugar cube please, or Selection strategies in the Buchberger algorithm", by Giovini, Mora, Niesi, Robbiano, Traverso

-- The point of sugar is, given fi, fj, to give an upper bound on the degree of sPoly fi fj without having to calculate it
-- We can then select by preference pairs with lower sugar, expecting therefore that the s-polys with have lower degree


addSugar i fi = (fi, i, degMP fi)



-- S-POLYNOMIALS

sPoly f g =
	let
		ltf = lt f
		ltg = lt g
		l = lcmTm ltf ltg
	in ((l `divTm` ltf) */ f) - ((l `divTm` ltg) */ g)


-- Note: doesn't test that it's a *reduced* basis
isGBasis gs = all (\s -> s %% gs == 0) (pairWith sPoly gs)



-- GROEBNER BASIS

-- [f xi xj | xi <- xs, xj <- xs, i < j]
pairWith _ [] = []
pairWith f (xi:xs) = map (\xj -> f xi xj) xs ++ pairWith f xs


createPair pi@(fi,i,si) pj@(fj,j,sj) = 
	let
		ti = lt fi
		tj = lt fj
		tij = lcmTm ti tj
		sij = max (si - degTm ti) (sj - degTm tj) + degTm tij -- the degree of sPoly fi fj is bounded above by this number
	in ((i,j),(fi,fj), tij, sij)

-- sPolyOfPair (_,(fi,fj),_,_) = sPoly fi fj
sPolyOfPair (_,(fi,fj),tij,_) =
	let
		ti = lt fi
		tj = lt fj
	in ((tij `divTm` ti) */ fi) - ((tij `divTm` tj) */ fj)

orderPair ((i,j),_,tij,sij) ((i',j'),_,tij',sij') = sij < sij' || (sij == sij' && j < j')

eliminatePairs1sloppy fk ls [] = reverse ls
eliminatePairs1sloppy fk ls (r:rs) =
	let
		(_,(fi,fj),tij,_) = r
		tik = lcmTm (lt fi) (lt fk)
		tjk = lcmTm (lt fj) (lt fk)
	in
		if tik `properlyDividesTm` tij && tjk `properlyDividesTm` tij
		then eliminatePairs1sloppy fk ls rs
		else eliminatePairs1sloppy fk (r:ls) rs

-- not currently used
eliminatePairs1fussy fk ls [] = reverse ls
eliminatePairs1fussy fk ls (r:rs) =
	let
		(_,(fi,fj),tij,sij) = r
		tik = lcmTm (lt fi) (lt fk)
		tjk = lcmTm (lt fj) (lt fk)
		sik = degTm tik
		sjk = degTm tjk
	in
		if tik `properlyDividesTm` tij && tjk `properlyDividesTm` tij && sik <= sij && sjk <= sij
		then eliminatePairs1fussy fk ls rs
		else eliminatePairs1fussy fk (r:ls) rs

termCoprime (_,as) (_,bs) = all (==0) (zipWith min as bs)

eliminatePairs2 ls [] = ls
eliminatePairs2 ls (r:rs) =
	let
		(_,(fi,fk),tik,_) = r
	in
		if (lt fi) `termCoprime` (lt fk)
		then
			let
				ls' = filter (\(_,_,tjk,_) -> snd tjk /= snd tik) ls
				rs' = filter (\(_,_,tjk,_) -> snd tjk /= snd tik) rs
			in eliminatePairs2 ls' rs'
		else eliminatePairs2 (r:ls) rs

eliminatePairs3fussy ls [] = reverse ls
eliminatePairs3fussy ls (r:rs) =
	let
		(_,_,tjk,_) = r
	in
		if any (canEliminate tjk) ls
		then eliminatePairs3fussy ls rs
		else eliminatePairs3fussy (r:ls) rs
	where
		canEliminate tjk (_,_,tik,_) = tik `dividesTm` tjk

groebnerBasis fs =
	let
		n = length fs
		fs' = zipWith addSugar [1..n] (map toMonicMP fs)
	in doGroebnerBasis n fs' (pairWith createPair fs')
	where
		doGroebnerBasis n gs [] = map (\(g,_,_) -> g) gs
		doGroebnerBasis n gs (pair:pairs) =
			let
				h = (sPolyOfPair pair) %% (map (\(g,_,_)->g) gs)
			in
				if termsMP h == []
				then doGroebnerBasis n gs pairs
				else
					let
						h' = addSugar (n+1) (toMonicMP h)
						pairs' = eliminatePairs1sloppy h [] pairs
						newpairs = map (\u -> createPair u h') gs
						newpairs' = mergeSort' orderPair (eliminatePairs2 [] newpairs)
						newpairs'' = eliminatePairs3fussy [] newpairs'
						pairs'' = merge' orderPair pairs' newpairs''
					in doGroebnerBasis (n+1) (gs++[h']) pairs''


-- REDUCED GROEBNER BASIS

-- given a groebner basis, find a minimal reduced basis
reduceGB gs = doReduceGB [] gs
	where
		doReduceGB gs' [] = gs'
		doReduceGB gs' (g:gs) =
			if any (`dividesTm` (lt g)) [lt h | h <- gs' ++ gs]
			then doReduceGB gs' gs
			else
				let (_,g') = quotRemMP g (gs' ++ gs)
				in doReduceGB (g':gs') gs

reducedGroebnerBasis fs = reduceGB (groebnerBasis fs)

gb fs = mergeSort' (>) (reducedGroebnerBasis fs)
-- !! should do the mergesort as we go along



-- LEADING TERM IDEAL

-- only valid if either the term order is graded, or <fs> is a homogeneous ideal
ltIdeal fs = map ltMP (gb fs)



x4 = x_ 4
x5 = x_ 5

cyclicRoots5 =
	[x1+x2+x3+x4+x5,
	x1*x2+x2*x3+x3*x4+x4*x5+x5*x1,
	x1*x2*x3+x2*x3*x4+x3*x4*x5+x4*x5*x1+x5*x1*x2,
	x1*x2*x3*x4+x2*x3*x4*x5+x3*x4*x5*x1+x4*x5*x1*x2+x5*x1*x2*x3,
	x1*x2*x3*x4*x5-1]