-- ellipticcurves.hs

module EllipticCurves where

-- import MPoly
import FF
import QQ
import Primes -- (primePowerFactors)
import NumberTheoryFundamentals (divides, power, intSqrt, legendreSymbol, sqrtsmodp)
import RedBlackTree
import RandomGenerator (randomInteger)
import PowerSeries
import UPoly
import IrreduciblePolyFp
import MathsPrimitives
import DirichletSeries hiding (($*))
import QuadraticField

-- Sources
-- Koblitz, Introduction to Elliptic Curves and Modular Forms
-- Ireland, Rosen, A Classical Introduction to Modern Number Theory (second edition)
-- Cohen, A Course in Computational Algebraic Number Theory


-- ELLIPTIC CURVES

data EllipticCurve a = EC a a deriving (Eq, Show)
-- EC a b represents the curve y^2 == x^3+ax+b
-- To be a valid elliptic curve, x^3+ax+b must not have repeated roots (the discriminant must not be zero)

data EllipticCurvePt a = Inf | P a a deriving (Eq, Ord, Show)
-- P x y

-- if the discriminant is zero, x^3+a*x+b has repeated roots
discriminantEC (EC a b) = -16 * (4 * a * a * a + 27 * b * b)
-- Cohen p372. This is -16 * the discriminant of the polynomial x^3+ax+b

-- !! Check sign - Ireland, Rosen use x^3-ax-b instead, so their sign may be wrong

isPtEC _ Inf = True
isPtEC (EC a b) (P x y) = y*y == x*x*x + a*x + b


-- ELLIPTIC CURVES OVER FP

ecfp p a b = EC (toFp p a) (toFp p b)

ptfp p x y = P (toFp p x) (toFp p y)

charECFp (EC (F p _) _) = p

ptsECFp (EC (F p a) (F _ b)) = Inf : [P (F p x) (F p y) | x <- [0..p-1], y <- sqrtsmodp (x*x*x + a*x + b) p]

countPtsECFp (EC (F p a) (F _ b)) = p + 1 + sum [legendreSymbol (x*x*x + a*x + b) p | x <- [0..p-1] ]

numPtsECFp curve@(EC (F p _) _)
	| p > 457 = findGpOrderECFp curve  -- see below
	| otherwise = countPtsECFp curve


-- GROUP LAW

-- Koblitz p34

-- assumes Fractional a
-- !! Not valid over F2 or F3
ecAdd _ Inf pt = pt
ecAdd _ pt Inf = pt
ecAdd (EC a b) (P x1 y1) (P x2 y2)
	| x1 /= x2 =
		let
			m = (y1-y2)/(x1-x2)
			x3 = m*m - x1 - x2
			y3 = - y1 + m * (x1 - x3)
		in P x3 y3
	| x1 == x2 =
		if y1 == -y2  -- includes the case y1 == y2 == 0
		then Inf
		else
			let
				m = (fromInteger 3 * x1 * x1 + a) / (fromInteger 2 * y1)
				x3 = m*m - 2*x1
				y3 = - y1 + m * (x1 - x3)
			in P x3 y3


ecMult ec k pt
	| k >= 0 = power (Inf, ecAdd ec) pt k
	| k < 0  = power (Inf, ecAdd ec) (ecNegate pt) (-k)

{-
ecMult _ 0 _ = Inf
ecMult _ _ Inf = Inf
ecMult ec k pt | k > 0 = doECMult Inf pt k
	where
		-- doECMult p q n = p + n * q
		doECMult p _ 0 = p
		doECMult p q n =
			let p' = if odd n then ecAdd ec p q else p
			    q' = ecAdd ec q q
			in doECMult p' q' (n `div` 2)
-}

ecNegate Inf = Inf
ecNegate (P x y) = P x (-y)


-- ORDER OF POINTS IN E(Fp)

-- The next two functions work in any group

-- Cohen p25
-- we are given that the order of g divides m
findOrderFromMultiple (idG,multG) m g = doFindOrder m (primePowerFactors m)
	where
		doFindOrder m [] = m
		doFindOrder m ((p,a):factors) =
			let
				m_p = m `div` (p^a)
				g' = power (idG,multG) g m_p
				m' = doFindLocalOrder p g' m_p
			in doFindOrder m' factors
		doFindLocalOrder p h n
			| h == idG = n
			| otherwise = doFindLocalOrder p (power (idG,multG) h p) (n*p)

-- we could perhaps do better using inverses
-- rather than calculating g^m_p each time, we could simply do g^m * (g^p^a)^-1


-- Cohen p240-2
-- Shanks' baby-step giant-step algorithm
-- given a g <- G, and an upper bound ub on its order, calculate its order. (We don't know the order of the group)

findOrderFromUpperBound (idG,multG,invG) ub g =
	let
		q = 1 + intSqrt ub
		(babySteps, g1:_) = splitAt (fromInteger q) (iterate (multG g) idG)
		babySteps' = rbfromlist (zip babySteps [0..])                                           -- == [1,g,g^2...g^(q-1)]
		g1' = invG g1
		giantSteps = zip [1..] (iterate (multG g1') g1')                                        -- == g^-q, g^-2q, ...
		(a,r) = head [(a,r) | (a,g1_a) <- giantSteps, Just r <- [babySteps' `rblookup` g1_a] ]  -- g^-aq == g^r, hence g^(aq+r) == 1
	in findOrderFromMultiple (idG,multG) (a*q+r) g
-- we know g^(aq+r) == idG, so aq+r is a multiple of the order


findPtOrderECFp _ Inf = 1
findPtOrderECFp curve@(EC a@(F p _) b) pt@(P x y) = findOrderFromUpperBound (Inf, ecAdd curve, ecNegate) (p + 1 + 2 * intSqrt p) pt

findPtOrderECFpTest curve@(EC a@(F p _) b) pt@(P x y) = head [k | (k,kpt) <- zip [1..] (iterate (ecAdd curve pt) pt), kpt == Inf]




-- ORDER OF GROUP E(Fp)

-- Cohen p375
-- We know that either E(Fp) is cyclic, or E(Fp) ~== Z/d1Z * Z/d2Z, with d1 | d2 and d1 | p-1

-- Also, from Hasse's theorem
-- p + 1 - 2 * sqrt p < # E(Fp) < p + 1 + 2 * sqrt p

-- Now, given d a quadratic non-residue mod p, consider the "anti-curve" E'(Fp) defined by y^2 == x^3 + a*d^2 x + b*d^3
-- The point about the anticurve is that d is a quadratic non-residue, and x^3+ad^2x+bd^3 = d^3 * ( (x/d)^3 + a (x/d) + b)
-- Hence each value of x gives rise either to two points on the curve and none on the anticurve, or vice versa, or to one on each (if x^3+ax+b == 0)
-- Hence if the number of points on the curve is p+1-a_p, then the number of points on the anticurve is p+1+a_p,

-- Then if E(Fp) ~== Z/d1Z * Z/d2Z, d1 | d2, and E'(Fp) ~== Z/d1'Z * Z/d2'Z, d1' | d2',
-- then for p > 457, we have max (d2, d2') > 4 sqrt p  -- Cohen p404
-- If we can find an elt of order > 4 sqrt p, then we are done, because # E(Fp) = d1 d2 lies in an interval of length 4 sqrt p, so this determines d1 uniquely


mix (a:as) (b:bs) = a : b : mix as bs
mix as [] = as
mix [] bs = bs


findGpOrderECFp curve@(EC a@(F p _) b) | p > 457 =
	let
		d = fromInteger (head [n | (n,_) <- iterate (\(_,seed) -> randomInteger p seed) (0,342349871), legendreSymbol n p == -1])
		anticurve = EC (a*d*d) (b*d*d*d)
		bound = intSqrt (16*p)  -- 4 * sqrt p
	in findGpOrder (findElt bound anticurve)
	where
		findElt bound anticurve@(EC a' b') =
			let
				ptorders = [(1, findPtOrderECFp curve (P x y)) | x <- map (F p) [0..p-1], F _ y2 <- [x*x*x+a*x+b], (y:_) <- [map (F p) (sqrtsmodp y2 p)]]
				antiptorders = [(-1, findPtOrderECFp anticurve (P x y)) | x <- map (F p) [0..p-1], F _ y2 <- [x*x*x+a'*x+b'], (y:_) <- [map (F p) (sqrtsmodp y2 p)]]
			in head (filter (\(sign,order) -> order > bound) (mix ptorders antiptorders))
		findGpOrder (sign, d2) =
			let
				d1 = (p + 1 + intSqrt (4*p)) `div` d2
				a_p = sign * (p+1-d1*d2)
			in p+1-a_p

-- We may be able to do more in the cases where the point we find is on the curve rather than the anticurve
-- eg if d1 and d2 are coprime, we know the group is cyclic
-- Is there a way that we can tell when it isn't cyclic?


-- ZETA FUNCTION OF E/Fp

zetaECFp curve
	| discriminantEC curve /= 0 =
		let
			p = fromInteger (charECFp curve)
			n1 = fromInteger (numPtsECFp curve)
			a = p + 1 - n1
		in (1 - a*t + p*t*t) / ((1-t) * (1-p*t))
	| otherwise = error "zetaECFp: characteristic divides discriminant"

numPtsECFqSeries curve = zipWith (*) (map fromInteger [0..]) (coeffsPS (log (zetaECFp curve)))


-- Ireland, Rosen, p302-3
-- Note that Cohen treats the case where p divides the discriminant slightly differently
localZetaEC :: EllipticCurve Integer -> Integer -> DirichletSeries QQ
localZetaEC curve@(EC a b) p =
	let
		n1 = fromInteger (numPtsECFp (ecfp p a b))
		a_p = p + 1 - n1
		p' = fromInteger p
		numerator =
			if p `divides` discriminantEC curve
			then 1
			else DS (1 : replicate (p'-2) 0 ++ fromInteger (-a_p) : replicate (p'^2-p'-1) 0 ++ fromInteger p : repeat 0)  -- == 1 - a_p p^-s + p p^-2s
		denominator = (eulerTermDS (p',1) * eulerTermDS (p',fromInteger p))                                               -- == 1/ ((1-p^-s)*(1-p p^-s))
	in numerator * denominator  -- eulerTermDS has already done reciprocal for us

globalZetaEC curve = fromPrimeProductDS [(fromInteger p, localZetaEC curve p) | p <- 2:[3,5..], isPrime p]


hasseWeilLfunctionEC curve = zeta s * zeta (s-1) / globalZetaEC curve


-- The following is slightly faster
_L_EC :: EllipticCurve Integer -> DirichletSeries QQ
-- _L_EC curve@(EC a b) = fromPrimeProductDS [(p, recip (term p)) | p <- 2:[3,5..], p' <- [toInteger p], isPrime p', not (p' `divides` discriminant)]
_L_EC curve@(EC a b) = recip (fromPrimeProductDS [(p, term p) | p <- 2:[3,5..], p' <- [toInteger p], isPrime p', not (p' `divides` discriminant)])
	where
		discriminant = discriminantEC curve
		term p = 
			let
				p' = toInteger p
				n1 = numPtsECFp (ecfp p' a b)
				a_p = fromInteger (p' + 1 - n1)
			in DS (1 : replicate (p-2) 0 ++ (-a_p) : replicate (p^2-p-1) 0 ++ fromInteger p' : repeat 0)  -- == 1 - a_p p^-s + p p^-2s



-- HECKE CHARACTERS

qch n x@(QF (-1) _ _) =
	if coprimeQF x (2*n')
	then let Q m 1 = normQF x in qch1 x * QF (-1) (fromInteger (legendreSymbol n m)) 0
	else QF (-1) 0 0
	where n' = QF (-1) (Q n 1) 0

qch1 x = head [i^j | j <- [0..3], (i^j * x) `modQF` (2+2*i) == 1]

heckeqch n x = x * qch n x


-- Koblitz p82
-- L(En,s) = (1/4) sum [heckeqch n x * (normQF x)^-s | x <- ZZ[i]\{0} ]

eltsZZi n = [fromInteger a + fromInteger b * i | a <- let x = intSqrt n in [-x..x], b <- let y = intSqrt (n-a*a) in if y == 0 then [0] else [-y,y], a*a+b*b == n]


-- L function for the curve y^2 = x^3 - n^2 x
_L_En n = DS (map (\i -> sum [heckeqch n x | x <- eltsZZi i] / 4) [1..])

-- testL n = _L_En n == hasseWeilLfunctionEC (EC (-n*n) 0)


-- COUNTING POINTS OVER Fq
-- counting points over extension fields of Fp

-- !! This code is inefficient
-- !! I need a better way to find sqrts mod Fq

allEltsFq f = map (toUPoly . map (F p)) (ptsAnFp p d)
	where
		p = charUPFF f
		d = degUP f

-- simple improvement to this - when you find the first sqrt, immediately work out the second as its negative, and stop
-- need to check for -x == x (only happens for 0, and in F2)


numPtsECFq curve p d = length (ptsECFq curve (findIrreduciblePolyFp p d))

ptsECFq (EC a b) f = Inf : [P x y | x <- allEltsFq f, y <- sqrtsFq f (x*x*x + a' * x + b')]
	where
		a' = toUPoly [a]
		b' = toUPoly [b]

sqrtsFq _ (UP []) = [UP []]
sqrtsFq f h =
	case [g | g <- allEltsFq f, (g*g-h) `modUP` f == UP []] of
	g : _ -> if g == -g then [g] else [g,-g]
	otherwise -> []
-- !! We need a better algorithm for finding sqrts in Fq


{-
-- Working over UPoly Integer instead makes things slightly faster, but of course it doesn't change the asymptotic complexity

ptsECFq' f (EC a b) = Inf : [P (toUPoly (map (F p) x)) y | x <- ptsAnFp p d, y <- sqrtsFq' f (x $* x $* x $+ a' $* x $+ b')]
	where
		p = charUPFF f
		d = degUP f
		a' = if a == 0 then [] else [a]
		b' = if b == 0 then [] else [b]

sqrtsFq' _ [] = []
sqrtsFq' f h =
	case [g | g <- ptsAnFp p d, toUPoly (map (toFp p) (g $* g $- h)) `modUP` f == UP []] of
	g : _ -> let g' = toUPoly (map (F p) g) in if g' == -g' then [g'] else [g',-g']
	otherwise -> []
	where
		p = charUPFF f
		d = degUP f
-}

-- FINITE GEOMETRY

-- needs to move into CombinatoricsGeneration
ptsAnFp p n = doAnFp [[]] 0
	where
		doAnFp pts i
			| i == n = pts
			| otherwise = doAnFp [x:xs | x <- [0..p-1], xs <- pts] (i+1)


-- TESTING

-- for p > 457, findGpOrderECFp should return same answer as countPtsECFp
-- so for a test, check that all of the following are true
-- [(p,a,b,countPtsECFp curve == findGpOrderECFp curve) | p <- filter isPrime [500..600],
--                                                      a <- [0..10], b <- [0..10],
--                                                      curve <- [ecfp p a b], discriminantEC curve /= 0]


eltOrdersECFp curve = map (findPtOrderECFp curve) (ptsECFp curve)







-- OLD CODE

{-

pointsFp curve p = [(x,y) | x <- map (toFp p) [0..p-1], y <- map (toFp p) [0..p-1], evalMP (curve /% p) [x,y] == 0]


pointsAnFp p 0 = [[]]
pointsAnFp p n = [x:xs | x <- map (toFp p) [0..p-1], xs <- pointsAnFp p (n-1)]

-- pointsA2Fp p =  [ [x,y] | x <- map (toFp p) [0..p-1], y <- map (toFp p) [0..p-1] ]

solutionsA2Fp curve p = [(x,y) | [x,y] <- pointsAnFp p 2, evalMP (curve /% p) [x,y] == 0]


pointsPnFp p 0 = [[1]]
pointsPnFp p n = map (1:) (pointsAnFp p n) ++ map (0:) (pointsPnFp p (n-1))

-- pointsP2Fp p = [ [1,x,y] | x <- map (toFp p) [0..p-1], y <- map (toFp p) [0..p-1]] ++ [ [0,1,x] | x <- map (toFp p) [0..p-1]] ++ [[0,0,1]]

solutionsP2Fp curve p = filter (\[t,x,y] -> evalMP (curve' /% p) [t,x,y] == 0) (pointsPnFp p 2)
	where curve' = toHomogeneous curve

-}