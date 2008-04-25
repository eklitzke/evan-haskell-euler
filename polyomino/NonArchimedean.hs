-- nonarchimedean.hs

module NonArchimedean where

import List (find, intersperse)
import NumberTheoryFundamentals (splitWith)
import MathsPrimitives (partialSums, partialProducts)
import QQ
import FF
import UPoly


-- Sources
-- Koblitz, p-adic Numbers, p-adic Analysis, and Zeta Functions
-- Robert, A Course in p-adic Analysis


-- P-ADIC NORM ON INTEGERS

ord_p_adic _ 0 = error "ord_p_adic _ 0"
ord_p_adic p n = if r == 0 then 1 + ord_p_adic p q else 0
	where (q,r) = divMod n p

norm_p_adic p 0 = 0
norm_p_adic p n = 1 / (fromInteger p ^ ord_p_adic p n) :: QQ


-- P-ADIC INTEGERS

-- p-adic integers are represented as an infinite list of integers in the range [0..p-1]
-- The list [a0,a1,...,an,...] represents a0 + a1*p + ... + an*p^n + ...

-- convert to normal form, with each coefficient in the range [0..p-1], by carrying or borrowing as necessary
normalZp p (a1:a2:as) = r : normalZp p (a2+q : as)
	where (q,r) = divMod a1 p

-- toZp p n = normalZp p (n : repeat 0)
toZp p 0 = repeat 0
toZp p n = r : toZp p q
	where (q,r) = divMod n p

addZp p (a0:a1:as) (b0:bs) = r : addZp p (a1+q:as) bs
	where (q,r) = divMod (a0+b0) p

negateZp p (0:as) = 0 : negateZp p as
negateZp p (a0:as) = (p-a0) : map (\a->p-1-a) as

subZp p as bs = addZp p as (negateZp p bs)

shift as = 0:as

multZp p (a0:as) bs = addZp p (map (a0*) bs) (shift (multZp p as bs))
-- the addZp call takes care of getting the coefficients back into the 0..p-1 range


-- recip of elt of Zp, using Newton's method - convergence is quadratic - the number of digits doubles at each step
recipZp p (0:_) = error "recipZp: not a unit"
recipZp p as@(a0:_) =
	let F _ b0 = recip (F p a0) -- so a0b0 == 1 (mod p)
	in b0 : henselLift [b0] 1
	where
		henselLift bs n =
			let
				x = bs ++ repeat 0
				x' = multZp p x (subZp p (2 : repeat 0) (multZp p as x)) -- x(2-ax)
				n' = 2*n  -- number of digits
			in take n (drop n x') ++ henselLift (take n' x') n'
		-- we want to find a solution to f(X) = aX-1 = 0
		-- however, we express this another way as g(X) = 1/X - a = 0
		-- then g'(X) = -1/X^2
		-- we then apply Newton's method, with the iteration
		-- X' = X - g(X)/g'(X) = X + X^2(1/X - a) = 2X - aX^2
		-- (Robert, Course in p-adic analysis, p49)
		-- Why are we guaranteed that the answer improves by one place on each iteration?


-- more straightforward but slower - using Hensel's lemma to converge a digit at a time
recipZp' p (0:_) = error "recipZp: not a unit"
recipZp' p as@(a0:_) =
	let F _ b0 = recip (F p a0) -- so a0b0 == 1 (mod p)
	in b0 : henselLift [b0] 1
	where
		henselLift bs n =
			let
				x = subZp p (1 : repeat 0) (multZp p as (bs ++ repeat 0))
				F _ b = F p (x !! n) / F p a0
			in b : henselLift (bs ++ [b]) (n+1)
		-- we know that [b_0..b_n-1] * [a0..a_n-1] == 1 (mod p^n)
		-- we want to find b_n such that [b_0..b_n] * [a0..a_n] == 1 (mod p^n+1)
		-- so b_n*p^n * as == 1 - [b_0..b_n-1]*[a_0..a..n] (mod p^n+1)
		-- so p^n coeff of 1 - bs*as = b_n * a0



-- newton's method to find a reciprocals of a mod p, p^2, p^4, p^8, ... Convergence is quadratic - we double the number of digits at each step
newtonRecips p a =
	let F _ x = recip (toFp p a)
	in doNewtonRecips (p*p) x
	where doNewtonRecips q x = x : doNewtonRecips (q*q) (x * (2-a*x) `mod` q)

-- to find the reciprocal of a p-adic integer which is in fact a natural number, we use newton's method
recipZpN p a =
	let segs = map (toZp p) (newtonRecips p a)
	in head (head segs) : doRecipZpN 1 (tail segs)
	where doRecipZpN n (seg:segs) = take n (drop n seg) ++ doRecipZpN (2*n) segs


-- Note that this will not terminate if called with zero
ordZp p (a:as)
	| a == 0    = 1 + ordZp p as
	| otherwise = 0

normZp p as = 1 / fromInteger p ^ ordZp p as

-- distance function
dZp p a b = normZp p (subZp p a b)


-- P-ADIC RATIONALS

data Qp = Qp Integer Int [Integer]
-- Qp p d as represents p^d * as
-- (so for p-adic rationals which aren't p-adic integers, d is negative)

precisionQp = 16 :: Int
-- most operations are done to infinite precision (or as far as you ask to see them)
-- the exceptions are
-- (==) - where we only test equality up to the specified precision
-- recip - where we throw division by zero error if the number is zero up to the specified precision
-- (we also use the specified precision to tell us when to cut off in the show function)

instance Eq Qp where
	x == y = let Qp _ _ as = x - y in all (==0) (take precisionQp as)


-- we try to start out with a0 non-zero, unless n itself is zero (which is represented by [0,0,0..]).
-- however, we can't insist that this is true for intermediate results (since they may be zero, but to find out we'd have to check an infinite number of places)
-- the exception is division (reciprocal) where we need non-zero a0.
-- So we check places up to the specified precision, looking for non-zero. If we don't find one, we conclude that the number is zero.

-- Note that if we add, subtract, or multiply two numbers, the output cannot be "bigger" than the biggest of the inputs
-- So if we do arithmetic involving only +, -, *, then there cannot be rounding errors
-- However, this is not true of recip

-- convert an integer to Qp
toQp' p 0 = Qp p 0 (repeat 0)
toQp' p n = Qp p s (toZp p t)
	where (s,t) = n `splitWith` p  -- so n == p^s * t

-- convert a rational to Qp
toQp p (Q n 1) = toQp' p n
toQp p (Q a b) =
	let (sa, ta) = a `splitWith` p
	    (sb, tb) = b `splitWith` p
	in Qp p (sa-sb) (multZp p (toZp p ta) (recipZpN p tb))

showQp' (Qp p d as) =
	let
		powers = map showTerm (filter (\(a,i) -> a /= 0) (zip (take precisionQp as) [d..]))
		finalterm = "O(" ++ show p ++ "^" ++ show (d + precisionQp) ++ ")"
	in
		concat (intersperse " + " (powers ++ [finalterm]))
	where showTerm (a,i) = show a ++ "*" ++ show p ++ "^" ++ show i

printQp x = putStrLn (showQp' x)

printQps [] = return ()
printQps (x:xs) = do {printQp x; printQps xs}


showQp numdigits (Qp p d as) = "Qp " ++ show p ++ "^" ++ show d ++ " " ++ show (take numdigits as)

instance Show Qp where
	show x = showQp precisionQp x

instance Num Qp where
	Qp p d as + Qp p' d' bs
		| p /= p'    = error "Qp.+: field mismatch"
		| d <= d'    = Qp p d (addZp p as (replicate (d'-d) 0 ++ bs))
		| otherwise  = Qp p d' (addZp p (replicate (d-d') 0 ++ as) bs)
	negate (Qp p d as) = Qp p d (negateZp p as)
	Qp p d as * Qp p' d' bs
		| p == p'    = Qp p (d+d') (multZp p as bs)
		| otherwise  = error "Qp.*: field mismatch"
	fromInteger _ = error "Qp.fromInteger: not defined"

instance Fractional Qp where
	recip x = case normalQp x of
		Just (Qp p d as) -> Qp p (-d) (recipZp p as)
		Nothing        -> error "recip.Qp: division by zero"

normalQp (Qp p d as) = doNormalQp 0 as
	where
		doNormalQp i (a:as)
			| i == precisionQp  = Nothing -- it looks like the number is zero, so give up
			| a == 0            = doNormalQp (i+1) as
			| otherwise         = Just (Qp p (d+i) (a:as))

-- Note that this will not terminate if called with zero
ordQp (Qp p d as) = d + ordZp p as

normQp x@(Qp p _ _) = 1.0 / fromInteger p ^ ordQp x

-- using 1/p is canonical, but to avoid overflows we are better off using 3/4
normQp' x@(Qp p _ _) = 0.75 ^ ordQp x

-- distance function
dQp a b = normQp (a - b)

dQp' a b = normQp' (a - b)


-- SOLUTIONS TO POLYNOMIALS

uptoQp :: Integer -> UPoly QQ -> UPoly Qp
uptoQp p (UP as) = UP (map (toQp p) as) 
-- !! Note that "show" doesn't work for UPoly Qp, because UPoly.show involves a "/= 0" test, which requires Qp.fromInteger

(UP as) /% p = UP (map (\(Q a b) -> toFp p a / toFp p b) as)

-- Hensel's lemma, Newton's method - converges quadratically, so number of digits doubles at each iteration
solvePolyQp p f =
	let
		fQp = uptoQp p f
		f' = derivUP f
		fQp' = uptoQp p f'
		seeds = [a | a <- [0..p-1], a' <- [toFp p a], evalUP (f /% p) a' == 0, evalUP (f' /% p) a' /= 0]
	in map (\a0 -> (Qp p 0 (a0 : doSolvePolyQp (fQp,fQp') [a0] 1)) ) seeds
	where doSolvePolyQp (f,f') as n =
		let
			x = Qp p 0 (as ++ repeat 0)
			Qp _ _ as' = x - evalUP f x / evalUP f' x
			as'' = take (2*n) as'
		in drop n as'' ++ doSolvePolyQp (f,f') as'' (2*n)

sqrtQp p n = solvePolyQp p (x^2 - fromInteger n)

-- !! Note a limitation of this code - the condition f' a /= 0 (mod p)
-- !! For example, this means that we can't find square roots in Q2
-- !! Even though in some cases there are perfectly valid solutions (eg sqrt 9 = 3, even in Q2)

-- !! Note also that sqrtQp 7 49 returns Nothing, which may indicate that we haven't implemented this in full generality

-- The Teichmuller representatives are 0 and the (p-1)th roots of unity
teichmullerRepsQp p = solvePolyQp p (x^p - x)
-- Koblitz p20 (ex 13)


-- P-ADIC POWER SERIES AND ELEMENTARY FUNCTIONS

untilConvergence (x1:x2:xs) = if ordQp (x1 - x2) > precisionQp then x2 else untilConvergence (x2:xs)
-- this doesn't guarantee the right answer - we might have x3 /= x2


-- log_p (1+x) = x - x^2/2 + x^3/3 - ... -- Koblitz p78
-- convergent when normQp p x < 1
logQp x@(Qp p _ _) = case normalQp (x - toQp p 1) of
	Nothing -> error "logQp: not convergent" -- x == 1
	Just x' ->
		if normQp x' < 1
		then
			let summands = zipWith (*) (iterate (*(-x')) x') (map (\n -> toQp p (1 / fromInteger n)) [1..])
			in untilConvergence (partialSums summands)
		else error "logQp: not convergent"

-- Robert p260 - The Iwasawa log is the unique continuation of log to Cp*.
-- It is given on Zp* by 1/(1-p) sum (1-x^(p-1))^k / k
iwasawaLog (Qp p _ as) = case normalQp (toQp p 1 - x^(p-1)) of
	Nothing -> toQp p 0 -- x == 1
	Just y  ->
		let summands = zipWith (*) (iterate (*y) y) (map (\n -> toQp p (1 / fromInteger n)) [1..])
		in untilConvergence (partialSums summands) * (toQp p (1 / fromInteger (1-p)))
	where x = Qp p 0 as
-- Koblitz p 87. log_p p = 0, log_p xy = log_p x + log_p y

-- exp_p x = 1 + x + x^2/2! + x^3/3! + ...
-- convergent when normQp x < (normQp p) ^ 1/(p-1)
-- (In Qp we don't have elts with norms which are fractional powers of p, so this is equivalent to normQp x < 1, unless p == 2 in which case normQp x < 1/2)
expQp x@(Qp p _ _) | (p == 2 && normQp x < 0.5) || normQp x < 1 = -- normQp' x < exp ( (1.0 / fromInteger (p-1)) * log (normQp' (toQp p (fromInteger p)) ) ) =
	let summands = zipWith (*) (iterate (*x) (toQp p 1)) (map (\n -> toQp p (1 / fromInteger n)) (partialProducts (1:[1..])))
	in untilConvergence (partialSums summands)
expQp _ | otherwise = error "expQp: not convergent"

-- logQp and expQp work as mutual inverses.
-- iwasawaLog agrees with logQp on the numbers for which they both converge
-- !! however expQp . iwasawaLog is not the identity. For example:
-- (expQp . iwasawaLog) (toQp 5 4) returns -4
-- (expQp . iwasawaLog) (toQp 5 7) returns sqrt (-49)
-- perhaps we need to be using the continuation of exp


