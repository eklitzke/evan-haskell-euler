-- quadraticfield.hs

module QuadraticField where

import QQ
import NumberTheoryFundamentals (divides, intSqrt, legendreSymbol, kroneckerSymbol, sqrtsmodp)
import Primes (isPrime, primePowerFactors, isSquareFree)
import DirichletSeries


-- QUADRATIC FIELDS

data QuadraticField = QF Integer QQ QQ deriving Ord
-- QF d a b represents a + b sqrt d

i = QF (-1) 0 1

w = (-1 + sqrtQF (-3)) / 2

sqrtQF d | isSquareFree (abs d) = QF d 0 1



instance Eq QuadraticField where
	x == y = let (QF _ a b, QF _ a' b', _) = fixQF x y in a == a' && b == b'
-- we are guaranteed that the ds are the same by the fixQF call


instance Show QuadraticField where
	show (QF _ 0 0) = "0"
	show (QF d x y) = showx ++ showplus ++ showy ++ showd
		where
			showx = if x == 0 then "" else show x
			showplus = if x == 0 || y <= 0 then "" else "+"
			showy = case y of
				-1 -> "-"
				0 -> ""
				1 -> ""
				otherwise -> show y
			showd = if y == 0 then "" else if d == -1 then "i" else "sqrt" ++ show d


fixQF x@(QF d a b) y@(QF d' a' b')
	| d == d' = (x,y,d)
	| d == 0  = (QF d' a b, y, d')
	| d' == 0 = (x, QF d a' b', d)
	| otherwise = error "fixQF: field mismatch"


instance Num QuadraticField where
	x + y = let (QF _ a b, QF _ a' b', d) = fixQF x y in QF d (a+a') (b+b')
	negate (QF d x y) = QF d (negate x) (negate y)
	x * y = let (QF _ a b, QF _ a' b', d) = fixQF x y in QF d (a*a' + fromInteger d*b*b') (a*b'+a'*b)
	fromInteger n = QF 0 (fromInteger n) 0

instance Fractional QuadraticField where
	recip z@(QF d x y) = QF d x' y'
		where
			norm = normQF z -- x*x - fromInteger d*y*y
			x' = x / norm
			y' = -y / norm
-- This correctly handles the d == 0 case

conjugateQF (QF d x y) = QF d x (-y)

normQF (QF d x y) = x * x - fromInteger d * y * y


-- discriminant of the field Q(sqrt d)
discriminantQF d
	| d' == 0    = error "discriminantQF: d is not squarefree"
	| d' == 1    = d
	| otherwise  = 4*d
	where d'  = d `mod` 4


-- RING OF INTEGERS

-- ring of integers is generated as ZZ-module by [1, generatorQF d]
generatorQF d = case d' of
	0         -> error "generatorQF: d is not squarefree"
	1         -> QF d (1/2) (1/2)
	otherwise -> QF d 0 1
	where d' = d `mod` 4

isIntegerQF (QF d a b) =
	(isIntegerQ a && isIntegerQ b) || (d `mod` 4 == 1 && isHalfIntegerQ a && isHalfIntegerQ b)

isUnitQF x = normQF x `elem` [1,-1]

unitsQF d
	| d == -1   = [1,i,-1,-i]
	| d == -3   = [1,w,w*w,-1,-w,-w*w]
	| d < 0     = [1,-1]
	| otherwise = error "unitsQF: not implemented for d > 0"

-- choose a canonical representative from among all associates of x
associateQF x@(QF d _ _)
	| d < 0     = maximum [x*u | u <- unitsQF d]
	| otherwise = max x (-x)

dividesQF d n = isIntegerQF (n/d)

-- Not intended to be efficient
-- Also, for d == -1, -3, no attempt at present to eliminate associates by i, w
findEltsQFNorm d n | d < 0 =
	if d `mod` 4 == 1
	then [QF d (toQ a 2) (toQ b 2) | a <- [0..2*sqrt_n+1], b <- [-2*sqrt_n_d-1..2*sqrt_n_d+1], even a == even b, a*a-d*b*b == 4*n]
	else [QF d (Q a 1) (Q b 1) | a <- [0..sqrt_n], b <- [-sqrt_n_d..sqrt_n_d], a*a-d*b*b == n]
	where
		sqrt_n = intSqrt n
		sqrt_n_d = intSqrt (n `div` abs d)


-- EUCLIDEAN ALGORITHM IN RING OF INTEGERS OF QUADRATIC FIELDS
-- Stewart and Tall, Algebraic Number Theory, p92ff
-- Not implemented for all norm-Euclidean quadratic fields yet.
-- (We only really need d == -1 and -3 cases)

-- The set of d for which the ring of integers of Q(sqrt d) is a Euclidean domain is
-- euclideanDomainsQF = [-1,-2,-3,-7,-11,2,3,5,6,7,11,13,17,19,21,29,33,37,41,57,73]

roundQ (Q x y) = (2*x+y-1) `div` (2*y)
-- note: roundQ (-1/2) == -1, whereas it should perhaps be zero

-- given an elt of Q(sqrt d), find the closest elt of its ring of integers
roundQF (QF d a b)
	| d `mod` 4 == 1 =
		let
			b' = toQ (roundQ (2*b)) 2
			a' = if isIntegerQ b' then Q (roundQ a) 1 else Q (roundQ (a+1/2)) 1 - 1/2
		in QF d a' b'
	| otherwise      = QF d (Q (roundQ a) 1) (Q (roundQ b) 1)


euclideanQF = [-1,-2,-3,-7,-11,2,3,5]  -- these are the only ones supported by our current algorithm

quotRemQF x y | d `elem` euclideanQF = quotRemQF' x' y'
	where (x',y',d) = fixQF x y

quotRemQF' x y = (q,r)
	where
		q = roundQF (x/y)
		r = x - y*q
-- This method only works for the listed values of d
-- This is because it relies on abs (normQF (roundQF (x/y) - (x/y))) < 1
-- which should guarantee that abs (normQF r) < abs (normQF y)


testQRQF x y = let (q,r) = quotRemQF' x y in r == 0 || abs (normQF r) < abs (normQF y)


divQF x y = z where (z,w) = quotRemQF x y

modQF x y = w where (z,w) = quotRemQF x y

gcdQF x 0 = x
gcdQF x y = let (q,r) = quotRemQF x y in gcdQF y r

coprimeQF x y = isUnitQF (gcdQF x y)


-- DECOMPOSITION OF PRIMES

data PrimeType = Inert | Split | Ramified deriving (Eq, Show)

-- primeType of p in QQ(sqrt d)
-- Borevich & Shafarevich, p238
primeType d 2
	| 2 `divides` d'   = Ramified
	| d' `mod` 8 == 1  = Split
	| d' `mod` 8 == 5  = Inert
	where d' = discriminantQF d
primeType d p =
	case legendreSymbol (discriminantQF d) p of
	0  -> Ramified  -- p | d
	-1 -> Inert     -- d is quadratic non-residue
	1  -> Split     -- d is quadratic residue


primeType' d p =
	case kroneckerSymbol (discriminantQF d) p of
	0  -> Ramified  -- p | d
	-1 -> Inert     -- d is quadratic non-residue
	1  -> Split     -- d is quadratic residue

-- !! Note: this is looking at all ideals, not just principal ideals
-- Where the ring of integers is not a PID, the ideals into which a prime splits may require two generators
-- For example, in Z[sqrt -5], there are no elements of norm 7.
-- Nevertheless, 7 splits, as <7,-4+sqrt-5>,<7,4+sqrt-5>


-- Cohen p224
decomposePrimeQF d p | isPrime p && isSquareFree (abs d) = case primeType d p of
	Ramified -> if p == 2 && d' `mod` 16 == 12 then [idealOf (QF d 1 0 + w)] else [idealOf w]
	Inert    -> [[p']]
	Split    ->
		let b = sqrtmod4p d' p
		in [idealOf (w - QF d (toQ (d'+b) 2) 0), idealOf (w - QF d (toQ (d'-b) 2) 0)]
	where
		d' = discriminantQF d
		p' = QF d (Q p 1) 0
		w = if d `mod` 4 == 1
		    then (fromInteger d + sqrtQF d) / 2
		    else (4*fromInteger d + 2*sqrtQF d) / 2
		idealOf q
			| q `dividesQF` p'     = [associateQF q]
			| d `elem` euclideanQF = [associateQF (gcdQF p' q)]
			| otherwise            = [p', associateQF q]
-- We do our best to reduce to a principal ideal
-- However, note that we can't currently do this for Euclidean domains where our simple Euclidean algorithm doesn't work
-- Also, we can't do it for UFDs which aren't Euclidean domains


sqrtmod4p a 2 = 1  -- !! Note, not true in general, but we know that d == 1 mod 8
sqrtmod4p a p = let b:_ = sqrtsmodp a p in if even a == even b then b else b+p


-- ZETA FUNCTION OF A QUADRATIC FIELD

-- from http://math.ucr.edu/home/baez/week216.html
-- we want to form the zeta function of the number field
-- for example, for ZZ(sqrt -1), we have
-- 2 ramified, 3 inert, 5 split
-- and the zeta function looks like product (1-2^-s)^-1 (1-3^-2s)^-1 (1-5^-s)^-1 (1-5^-s)^-1
-- (it's the product of (1-|P|^-s) for the norms of all prime ideals)
-- (in the 2, ramified case, 2 = (1+i)(1-i), but these both belong to the same ideal <1+i>, which has norm 2)
-- (in the 3, inert case, 3 belongs to <3>, which has norm 3^2)
-- (in the 5, split case, 5 = (2+i)(2-i), and we have two ideals <2+i>, <2-i> of norm 5)

-- (The zeta function can also be expressed as sum |I|^-s over all ideals)


-- Borevich & Shafarevich, p309-10

zetaCoeffQF _ 1 = 1
zetaCoeffQF d n = product (map contribution (primePowerFactors n))
	where
		contribution (p,a) = case primeType d p of
			Inert    -> if even a then 1 else 0
			Ramified -> 1
			Split    -> a+1

zetaQF d = fromPrimeProductDS [(p, pSeries p) | p <- 2:[3,5..], isPrime (toInteger p)]
	where
		pSeries p = case primeType d p of
			Inert    -> eulerTermDS (p*p,1)
			Ramified -> eulerTermDS (p,1)
			Split    -> (eulerTermDS (p,1))^2


-- UNFINISHED CODE
-- DECOMPOSITION OF PRIMES IN UFDS

decomposePrimeUFD qn@(QF d (Q p 1) 0) | d `elem` [-1,-2,-3,-7,-11,-19,-43,-67,-163] && isPrime p = decomposePrimeUFD' qn

decomposePrimeUFD' q@(QF d (Q p 1) 0) = case primeType d p of
	Ramified -> [findFactor d p]
	Inert -> [q]
	Split -> let z = findFactor d p in [z, conjugateQF z]
	where
		findFactor d p =
			let
				w = generatorQF d
				normw = normQF w
			in head [z | a <- [0..intSqrt p], b <- [0..intSqrt p], z <- [fromInteger a + fromInteger b * w], normQF z == fromInteger p]
-- !! This can certainly be improved
-- !! Use Cornacchia's algorithm (from Cohen book)

