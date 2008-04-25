-- schreiersims.hs

module SchreierSims where

import CombinatoricsGeneration (cartProd)
import RedBlackTree
import RandomGenerator
import PermutationGroups


-- Both implementations are taken from Seress, Permutation Group Algorithms


-- COSET REPRESENTATIVES FOR STABILISER OF A POINT

-- coset representatives for Gx (stabiliser of x) in the group G = <gs>
-- in other words, for each x' in the orbit of x under G, we find a g <- G taking x to x'
-- the code is basically just the code for calculating orbits, but modified to keep track of the group elements that we used to get there
cosetRepsGx gs x = doCosetRepsGx gs (rbfromlist [x'], [x'])
	where x' = (x, 1) -- coset representative for x itself is the identity

doCosetRepsGx gs (reps, []) = reps
doCosetRepsGx gs (reps, lastreps) =
	let
		newreps = [(y .^ g, h * g) | (y,h) <- lastreps, g <- gs]
		newreps' = filter (\(y,h) -> reps `rblookup` y == Nothing) newreps -- don't store a g-path to y if we already have a shorter one
	in doCosetRepsGx gs (foldl rbupdate reps newreps', newreps')
-- newreps' may itself contain duplicates, so some of the updates may be overwrites


-- update coset representatives for the addition of a new generator.
-- (gs are the existing generators, g the new generator)
updateCosetRepsGx gs g t =
	let
		newreps = [(y .^ g, h * g) | (y,h) <- rbtolist t]
		newreps' = filter (\(y,h) -> t `rblookup` y == Nothing) newreps
	in doCosetRepsGx (g:gs) (foldl rbupdate t newreps', newreps')


-- RANDOM SCHREIER-SIMS ALGORITHM

-- !! WARNING - You need to check that what you've got at the end IS the full group
-- !! If you know the expected order, this is easy
-- !! Another way is to chuck more random elts at it, and check that they sift
-- (We ought to provide a way to continue the algorithm)

-- if (bs,gs) are a base and strong generating set, then this will calculate the Schreier-Sims transversals
-- if (bs,gs) aren't a base and strong generating set, then when we sift random group members through, some will fail to sift
-- we can use these to augment gs
schreierSimsStructure _ (_,[]) = []
schreierSimsStructure n ([],gs) =
	let
		Just b = findBase n gs
		gs' = filter (\g -> b .^ g == b) gs
		t = cosetRepsGx gs b
	in (b,gs,t) : schreierSimsStructure n ([],gs')
schreierSimsStructure n (b:bs,gs) =
	let
		gs' = filter (\g -> b .^ g == b) gs
		t = cosetRepsGx gs b
	in (b,gs,t) : schreierSimsStructure n (bs,gs')


-- update Schreier-Sims structure for addition of a new generator
updateSchreierSimsStructure n (True,h) structure =
	let structure' = schreierSimsStructure n ([],[h])
	in structure ++ structure'
updateSchreierSimsStructure n (False,h) ((b,s,t):structure) =
	if b .^ h == b
	then (b,s,t) : updateSchreierSimsStructure n (False,h) structure
	else
		let t' = updateCosetRepsGx s h t
		in (b, h:s, t') : structure

randomSchreierSimsTransversals n gs seed =
	let
		structure = randomSchreierSimsAlgorithmGivenBase n [] gs seed
		(bs,_,ts) = unzip3 structure
	in (bs, ts)

randomSchreierSimsAlgorithmGivenBase n bs gs seed =
	let
		hs = drop 2 (randomWalkOnCayleyGraph (length gs, gs) (identity n, seed)) -- the first two steps are the identity and one of the generators
		structure = doRandomSchreierSimsAlgorithm (gs,schreierSimsStructure n (bs,gs), hs,20)
	in structure
	where
		doRandomSchreierSimsAlgorithm (s, structure, h:hs,0) = structure
		doRandomSchreierSimsAlgorithm (s, structure, h:hs,i) =
			let (bs,_,ts) = unzip3 structure
			in case sift (bs,ts) h of
			Nothing -> doRandomSchreierSimsAlgorithm (s, structure, hs, i-1)
			Just (through,h') -> 
				let structure' = updateSchreierSimsStructure n (through,h') structure
				in doRandomSchreierSimsAlgorithm (h':s, structure', hs, 20)

randomSchreierSimsTransversalsGivenBase n bs gs seed =
	let
		structure = randomSchreierSimsAlgorithmGivenBase n bs gs seed
		(bs',_,ts) = unzip3 structure
	in (bs', ts)


-- SCHREIER-SIMS ALGORITHM


-- generators for the stabiliser of x in the group G = <gs>
-- Schreier's Lemma states that if <S> = H < G, then, and R a set of coset reps for H in G
-- then { rs(rs)*^-1 | r <- R, s <- S } generates H (where * means "the coset representative of")
schreierGeneratorsGx n gs (reps,x) = toSet (filter (not . isIdentity) [schreierGenerator r g | r <- map snd (rbtolist reps),  g <- gs])
	where
		schreierGenerator r g =
			let
				h = r * g
				Just h' = reps `rblookup` (x .^ h)
			in h * inverse h'

sift (bs,ts) g = doSift (bs,ts) g
	where
		doSift ([],[]) g =
			if isIdentity g
			then Nothing
			else Just (True,g) -- we sifted through, but didn't get the identity, so we need to add another base element
		doSift (b:bs,t:ts) g =
			case t `rblookup` (b .^ g) of
			Nothing -> Just (False,g)
			-- Just h -> doSift (bs,ts) (mult (inverse h) g)
			Just h -> doSift (bs,ts) (g * inverse h)

findBase n (g:_) =
	let moved = [i | i <- [1..n], i .^ g /= i]
	in if null moved then Nothing else Just (head moved)

schreierSimsAlgorithm n gs =
	let
		Just b = findBase n gs
		t = cosetRepsGx gs b
		sgs = schreierGeneratorsGx n gs (t,b)
	in
		doSchreierSimsAlgorithm ([(b,gs,t)], []) [sgs]
	where
		doSchreierSimsAlgorithm ([], structure) _ = structure
		doSchreierSimsAlgorithm (bad:bads, goods) ([]:sgs) = doSchreierSimsAlgorithm (bads,bad:goods) sgs
		doSchreierSimsAlgorithm (bads, goods) ((h:hs):sgs) =
			let
				(bs,_,ts) = unzip3 goods
			in
				case sift (bs,ts) h of
				Nothing -> doSchreierSimsAlgorithm (bads, goods) (hs : sgs)
				Just (_,h') ->
					if null goods
					then
						let goods' = schreierSimsAlgorithm n [h']
						in doSchreierSimsAlgorithm (bads, goods') (hs : sgs)
					else
						let
							(b,s,t) = head goods
							s' = h':s
							t' = updateCosetRepsGx s h' t
							-- t' = cosetRepsGx n s' b
							newsgs = schreierGeneratorsGx n s' (t',b)
						in doSchreierSimsAlgorithm ((b,s',t') : bads, tail goods) (newsgs : hs : sgs)

schreierSimsTransversals n gs =
	let (bs,_,ts) = unzip3 (schreierSimsAlgorithm n gs)
	in (bs, ts)


-- USING THE SCHREIER-SIMS TRANSVERSALS

isMemberSS (bs,ts) g = sift (bs,ts) g == Nothing

-- By Lagrange's thm, every g <- G can be written uniquely as g = r_m ... r_1 (Seress p56)
-- Note that we have to reverse the list of coset representatives
eltsSS (_,ts) = map (product . reverse) (cartProd ts')
	where ts' = map rbvalues ts

orderSS (_,ts) = product (map (toInteger . rbcount) ts)


randomEltSS (_,ts) seed =
	let
		ts' = map rbvalues ts
		ns = map length ts'
	in randomEltSS' (ns,ts') seed

randomEltSS' (ns,ts) seed = doRandomElt (ns,ts) [] seed
	where
		doRandomElt (n:ns,t:ts) rs seed =
			let
				(i,seed') = randomInt n seed
				r = t !! i
			in doRandomElt (ns,ts) (r:rs) seed'
		doRandomElt ([],[]) rs seed = (product rs, seed)
-- Note that we don't need to reverse the rs at the end because we did it as we went along


randomEltsSS (_,ts) seed =
	let
		ts' = map rbvalues ts
		ns = map length ts'
	in map fst (tail (iterate (\(_,seed) -> randomEltSS' (ns,ts') seed) (identity 0, seed)))


{-
onePointStabilizer (b:bs,_:rs) = (bs, map (map (stabilize b)) rs)
	where
		stabilize b g = map (\i -> if i < b then i else i-1) (take (b-1) g ++ drop b g)
-}

