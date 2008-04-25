-- combinatoricscounting.hs

module CombinatoricsCounting where

import MathsPrimitives (partialProducts, ($+), ($-), ($.) )
import QQ
import UPoly


-- FACTORIALS

factorial n = product [1..toInteger n]

-- fallingFactorial _ 0 = 1 -- this line unnecessary as product [] == 1
fallingFactorial x n = product [x - fromInteger i | i <- [0..toInteger n - 1] ]
-- == factorial k `div` factorial (k-n)

-- risingFactorial _ 0 = 1
risingFactorial x n = product [x + fromInteger i | i <- [0..toInteger n - 1] ]


-- BINOMIAL COEFFICIENTS

choose n k = fallingFactorial n k `div` factorial k

binomial n k = choose n k

nextPascal xs = xs $+ (0:xs)

pascalTriangle = iterate nextPascal [1]

choose' n k = pascalTriangle !! n !! k
-- choose' is only slightly slower than choose, and will be quicker for multiple calls, when pascalTriangle will have already been calculated


-- STIRLING NUMBERS OF THE FIRST KIND

-- van Lint and Wilson, A Course in Combinatorics (2nd ed), Ch 13
-- Cameron, Combinatorics, p80ff

-- s(n+1,k) == -n s(n,k) + s(n,k-1)
nextStirlingFirsts (n,xs) = (n+1, (0:xs) $- map (n*) xs)

stirlingFirstTriangle = map snd (iterate nextStirlingFirsts (0,[1]))

stirlingFirst n k = stirlingFirstTriangle !! n !! k


-- SIGNLESS STIRLING NUMBERS OF THE FIRST KIND
-- == number of permutations of n elts having k cycles

nextSignlessStirlingFirsts (n_1,xs) = (n_1 + 1, map (n_1*) (xs) $+ (0:xs) )

signlessStirlingFirstTriangle = map snd (iterate nextSignlessStirlingFirsts (0,[1]))

signlessStirlingFirst n k = signlessStirlingFirstTriangle !! n !! k


-- STIRLING NUMBERS OF THE SECOND KIND
-- == the number of partitions of an n-set into k blocks

-- S(n+1,k) == k S(n,k) + S(n,k-1)
nextStirlingSeconds xs = zipWith (*) [0..] (xs) $+ (0:xs)

stirlingSecondTriangle = iterate nextStirlingSeconds [1]

stirlingSecond n k = stirlingSecondTriangle !! n !! k


-- BELL NUMBERS

-- the number of partitions of an n-set (where [[1],[2,3]] and [[1,2],[3]] are different partitions)
-- in other words, sum [stirlingSecond n k | k <- [1..n]]
bellNumber' n = sum (stirlingSecondTriangle !! n)

-- Bn = sum [(n-1) `choose` (i-1) * Bn-i | i <- [1..n]] , B0 = 1
-- we calculate the choose coefficients as we go along using the usual recurrence relation (this is much more efficient than calling choose)
bellNumbers = 1 : doBellNumbers ([1],[1])
	where doBellNumbers (cs,bs) =
		let
			cs' = nextPascal cs -- cs $+ (0:cs)
			b = cs $. bs -- sum (zipWith (*) cs bs)
		in b: doBellNumbers (cs',b:bs)

bellNumber n = bellNumbers !! n


-- CATALAN NUMBERS

catalan n
	| n == 0    = 0
	| n == 1    = 1
	| otherwise = ((2*n-2) `choose` (n-1)) `div` n

catalanNumbers = 0 : 1 : doCatalanNumbers [1]
	where doCatalanNumbers cs =
		let c = cs $. (reverse cs) -- sum (zipWith (*) cs (reverse cs))
		in c : doCatalanNumbers (c:cs)

catalan' n = catalanNumbers !! n


-- BERNOULLI NUMBERS

-- Ireland, Rosen, A Classical Introduction to Modern Number Theory, p229ff
-- We can calculate the Bernoulli numbers using the recurrence
-- sum [(m+1) `choose` k * bernoulli k | k <- [0..m] ] == 0
-- eg B0 + 4 B1 + 6 B2 + 4 B3 == 0 (where B0 == 1)
bernoulliNumbers :: [QQ]
bernoulliNumbers = 1 : doBernoulliNumbers [1,2,1] [1] 1
	where
		doBernoulliNumbers cs bs m =
			let b = - (cs $. bs) / (m+1)
			in b : doBernoulliNumbers (nextPascal cs) (bs ++ [b]) (m+1)

bernoulliNumber k = bernoulliNumbers !! k

-- b_n = sum [(-1)^k * k! * S(n,k) / (k+1) | k <- [1..n] ]  -- Cameron, p85
bernoulliNumber' n = sum [toQ ((-1)^k * factorial k * stirlingSecond (fromInteger n) (fromInteger k)) (k+1) | k <- [1..n] ] :: QQ


-- (UNORDERED) PARTITIONS

num_kPartitions n k
	| k <= 0    = 0
	| n < k     = 0
	| n == k    = 1
	| otherwise = num_kPartitions (n-1) (k-1) + num_kPartitions (n-k) k


-- CODE VARIANTS

-- Stirling numbers of the first kind (resp. signless), correspond to the coefficients of falling (resp. rising) factorials

fallingFactorials = 1 : partialProducts [x - fromInteger n | n <- [0..] ]

risingFactorials = 1 : partialProducts [x + fromInteger n | n <- [0..] ]

stirlingFirstTriangle' = map (\(UP as) -> as) fallingFactorials

signlessStirlingFirstTriangle' = map (\(UP as) -> as) risingFactorials

stirlingFirst' n k = let UP as = fallingFactorial x n in as !! k

signlessStirlingFirst' n k = let UP as = risingFactorial x n in as !! k

