-- powerseries.hs

module PowerSeries (PowerSeries (..), t, coeffsPS, coeffsPS', leadingPS, (~=), (*/),
                    diagonalSumPS, inversePS,
					binomial1, (^%),
					binomialPS, expPS, log1PS,
					fromOGF, fibonacciOGF, catalanOGF, ramanujanTauOGF,
					fromEGF, bernoulliEGF, bellEGF) where

import List (intersperse)
import MathsPrimitives (FunctionRep (..), partialProducts, factorials, ($+), ($.) )
import QQ


-- Sources
-- http://en.wikipedia.org - "trigonometric functions", "hyperbolic functions"
-- Doug McIlroy, "Power Series, Power Serious"
-- Dan Piponi, code and email

infixr 8 ^%

newtype PowerSeries = PS [QQ]


coeffsPS f = coeffsPS' f ++ repeat 0

coeffsPS' (PS as) = as

-- These are formal power series - we're not interested in questions of convergence

PS as ~= PS bs = take 10 as == take 10 bs

instance Eq PowerSeries where
	f == g = error "PowerSeries.== : not defined, use ~= instead"



-- SHOW INSTANCE

instance Show PowerSeries where
	show f = showLeading 10 f ++ "+..."

-- tries to show the first n non-zero terms, by looking at the first 2n terms
showLeading n (PS as) =
	let powers = take n (filter (\(a,i) -> a /= 0) (zip as [0..2*n]))
	in
		if null powers
		then "0"
		else foldl1 linkTerms (map showTerm powers)
	where linkTerms t u = if head u == '-' then t ++ u else t ++ "+" ++ u

showTerm (a,0) = show a
showTerm (a,i) = showCoeff a ++ showPower i

showCoeff a
	| a == 1    = ""
	| a == -1   = "-"
	| otherwise = show a

showPower i
	| i == 0    = ""
	| i == 1    = "t"
	| otherwise = "t^" ++ show i

leadingPS n f = print $ showLeading n f


-- NUM INSTANCE

instance Num PowerSeries where
	PS as + PS bs = PS (as $+ bs)
	negate (PS as) = PS (map negate as)
	PS as * PS bs = PS (as $* bs)
	fromInteger n = PS [fromInteger n]

(a:as) $* (b:bs) = [a*b] $+ (0 : map (a*) bs) $+ (0: map (*b) as) $+ (0 : 0 : (as $* bs))
_ $* _ = []


t = PS [0,1]

-- sum an infinite sequence of power series, on the assumption that each has at least one more leading zero than the previous
diagonalSumPS fs = PS (dsum 0 [] fs)
    where dsum i as (PS bs : fs) = let bs' = drop i bs
                                       j = length $ takeWhile (==0) bs'
                                   in take j (as ++ repeat 0) ++ dsum (i+j) (drop j (as $+ bs')) fs
          dsum i as [] = as


-- FRACTIONAL INSTANCE

instance Fractional PowerSeries where
	recip (PS as) = PS (recipPS as)
	PS (0:as) / PS (0:bs) = PS as / PS bs
	f / g = f * recip g
	-- denominator must not be divisible by t, unless numerator also is, in which case we can cancel

recipPS (0:_) = error "PowerSeries.recip: a0 == 0"
recipPS (a:as) = (1/a) : doRecip [1/a]
	where doRecip bs = let b = - (as $. bs) / a in b : doRecip (b:bs)

-- [a0,a1,a2,...] * [b0,b1,b2,...] = [a0b0, a0b1+a1b0, a0b2+a1b1+a2b0, ...]
-- For bs == recip as, we want as * bs = [1,0,0,...]
-- So a0b0 == 1, a0b1+a1b0 == 0, a0b2+a1b1+a2b0 == 0, ...
-- So b0 == 1/a0, b1 == (-1/a0) a1b0, b2 == (-1/a0) (a1b1+a2b0), ...


c */ as = map (c*) as

recipPS' (0:_) = error "PowerSeries.recip: a0 == 0"
recipPS' (a:as) = 1/a : bs
	where bs = (-1/a) */ ( ((1/a) */ as) $+ (0 : (as $* bs)) )
-- (a + t as)(b + t bs) == 1
-- => ab + t (a bs + b as) + t^2 (as bs) == 1
-- => ab == 1 and t a bs == - t b as - t^2 as bs
-- => the recursion bootstraps because to calculate the nth coeff of bs is dependent only on the first n-1 (or put another way, because of the 0: in the expression)


-- The running times of recipPS and recipPS' are about the same


-- COMPOSITION OF POWER SERIES

composePS (PS as) (PS (0:bs)) = PS (doCompose as [1] [])
	where
		doCompose (a:as) power sum =
			let s:sum' = sum $+ map (a*) power
			in s : doCompose as (bs $* power) sum'
		doCompose [] _ sum = sum
composePS _ _ = error "compose: second argument has non-zero constant term"

-- if the second argument has non-zero constant term, then we would need to sum an infinite series to find the first term of the composite power series
-- this is not valid for *formal* power series
-- (we could do it for power series over R, say, but the code would be significantly more complicated)

-- adapted from McIlroy's version - empirically, this appears to be less efficient than my version
composePS' (PS as) (PS bs) = PS (doCompose' as bs)
	where
		doCompose' (a:as) (0:bs) = a : bs $* doCompose' as (0:bs)


-- adapted from McIlroy
-- the functional inverse - given f, find g such that f(g(t)) == t == g(f(t))
inversePS (PS (0:as)) = PS (0:bs)
	where PS bs = recip (composePS (PS as) (PS (0:bs)))


-- CALCULUS

derivPS (PS []) = PS []
derivPS (PS (_:as)) = PS (zipWith (*) (map fromInteger [1..]) as)
-- This exactly parallels the definition of derivUP

integPS (PS as) = PS (0 : zipWith (/) as (map fromInteger [1..]))


instance FunctionRep PowerSeries where
	compose = composePS
	deriv = derivPS
	integ = integPS




-- CALCULATING POWER SERIES FOR TRANSCENDENTAL FUNCTIONS
-- If we didn't already know the power series expansions for the elementary transcendental functions,
-- we could now calculate them from their known integro-differential properties

-- from McIlroy
exp_t :: PowerSeries
exp_t = 1 + integ exp_t

log1_t :: PowerSeries
log1_t = integ (1/(1+t))

sin_t :: PowerSeries
sin_t = integ cos_t

cos_t :: PowerSeries
cos_t = 1 - integ sin_t

tan_t = sin_t / cos_t

arcsin_t :: PowerSeries
arcsin_t = integ (sqrt (1/(1-t^2)))

arctan_t :: PowerSeries
arctan_t = integ (1 / (1+t^2))

arctanh_t :: PowerSeries
arctanh_t = integ (1 / (1-t^2))


-- EXPLICIT POWER SERIES FOR TRANSCENDENTAL FUNCTIONS
-- We can construct power series for transcendental functions explicitly.
-- (This is useful for testing, and is usually the fastest way to get these power series if we want to use them in composition)


-- Koblitz 81-2
-- (1+t)^a == sum a(a-1)...(a-n+1)/n! x^n  -- valid for all a <- QQ
binomialPS :: QQ -> PowerSeries
binomialPS a =
	let numerators = partialProducts (map (\m -> a - fromInteger m) [0..])
	    denominators = partialProducts (map fromInteger [1..])
	in PS (1 : zipWith (/) numerators denominators)

binomial1 a f@(PS(0:_)) = composePS (binomialPS a) f
binomial1 _ _ = error "binomial1: only defined when a0 == 0"

-- power a f = binomial1 a (f-1)

f ^% a = binomial1 a (f-1)
-- Note that for whole numbers f ^ n is faster than f ^% n

expPS = PS (map (Q 1) factorials)

-- Note: this is the power series for log(1+t), not log t
log1PS = PS (0 : zipWith Q alternating [1..])
	where alternating = 1 : (-1) : alternating

sinPS = PS (zipWith toQ numerators factorials)
	where numerators = 0 : 1 : 0 : (-1) : numerators

cosPS = PS (zipWith toQ numerators factorials)
	where numerators = 1 : 0 : (-1) : 0 : numerators

tanPS = sinPS / cosPS


arcsinPS =
	let numerators = 0 : intersperse 0 (1 : partialProducts [1,3..])
	    denominators = zipWith (*) (1 : intersperse 1 (1 : partialProducts [2,4..])) (1:[1..])
	in PS (zipWith toQ numerators denominators)

arctanPS = PS (zipWith toQ numerators (1 : [1..]) )
	where numerators = 0 : 1 : 0 : (-1) : numerators

sinhPS = PS (zipWith toQ numerators factorials)
	where numerators = 0 : 1 : numerators

coshPS = PS (zipWith toQ numerators factorials)
	where numerators = 1 : 0 : numerators

tanhPS = sinhPS / coshPS

arcsinhPS =
	let signs = 0 : 1 : 0 : (-1) : signs
	    numerators = zipWith (*) signs (0 : intersperse 0 (1 : partialProducts [1,3..]))
	    denominators = zipWith (*) (1 : intersperse 1 (1 : partialProducts [2,4..])) (1:[1..])
	in PS (zipWith toQ numerators denominators)

arctanhPS = PS (zipWith toQ numerators (1 : [1..]))
	where numerators = 0 : 1 : numerators



-- FLOATING INSTANCE
-- There are several different ways that each function could be defined. We have chosen the fastest.

-- !! Warning - some of the following are only valid for first term == 0 or == 1
-- We need to work out which these are, and put appropriate guards in

instance Floating PowerSeries where

	pi = error "PowerSeries.pi : not defined"

	exp f = compose expPS f

	log f = integ (deriv f / f)
	-- from Dan - follows from d/dx (log (f(x)) = f'(x) / f(x)

	sqrt (PS (0:0:as)) = PS (0:bs) where PS bs = sqrt (PS as)
	sqrt f@(PS (1:_)) = g
		where g = 1 + integ (deriv f / (2 * g))
	sqrt f = error ("PowerSeries.sqrt not defined for " ++ show f)
	-- from McIlroy

	sin f = compose sinPS f -- doSin f (deriv f)

	cos f = compose cosPS f -- doCos f (deriv f)

	sinh f = compose sinhPS f -- doSinh f (deriv f)

	cosh f = compose coshPS f -- doCosh f (deriv f)

	asin f	= integ (deriv f / sqrt(1-f*f))
		-- from Dan. Justification:
		-- f == sin g
		-- => f' == g' cos g (differentiation with chain rule)
		-- == g' sqrt (1 - (sin g)^2)
		-- == g' sqrt (1 - f^2)
		-- => g' = f' / sqrt (1-f^2)

	atan f = integ (deriv f / (1+f*f))

	acos _ = error "PowerSeries.acos : not defined" -- acos f == pi/2 - asin f, so could be defined as PowerSeries RR, but not as PowerSeries QQ

	asinh f	= integ (deriv f / sqrt(1+f*f))

	atanh f = integ (deriv f / (1-f*f))

	acosh _ = error "PowerSeries.acosh : not defined"



sin' f = integ (f' * cos' f) where f' = deriv f
cos' f = 1 - integ (f' * sin' f) where f' = deriv f



-- GENERATING FUNCTIONS

fromEGF (PS as) = zipWith (*) as (map fromInteger factorials)

fromOGF (PS as) = as


fibonacciOGF = 1 / (1-t-t^2)

-- The Catalan numbers count the number of binary trees, which satisfy T = 1 + x T^2
catalanOGF = PS ts
	where ts = 1 : ts $* ts

-- The ogf for the Catalan numbers comes from solving x T^2 - T + 1 == 0
-- => sum c_k t^k = (1 - sqrt (1-4t)) / 2t
catalanOGF' = (1 - sqrt (1 - 4*t)) / (2*t)


-- egf for the Bernoulli numbers is sum (b_k t^k / k!) = t/(e^t-1)
bernoulliEGF = t/(exp t - 1)
-- == t / (expPS - 1)
-- if we interpret this as a function of D, then we find that D/(e^D-1) (x^n) = B_n(x), nth Bernoulli poly

-- egf for the Bernoulli polys t e^xt / (e^t-1)

-- egf for Bell numbers is sum (b_k t^k / k!) = e^(e^t-1)
bellEGF = exp (exp t - 1)
-- == exp (expPS - 1)


-- Koblitz p122
-- Ramanujan tau function is defined by sum [tau(n) * q^n | n <- [1..] ] == q * product [(1-q^n)^24 | n <- [1..] ]
ramanujanTauOGF = t * firstProduct ^ 24
	where
		firstProduct =
			let iterates = partialProducts [(1-t^n) | n <- [1..] ]
			in PS (1 : zipWith (\f i -> coeffsPS f !! i) iterates [1..])
-- firstProduct == product [1-q^n | n <- [1..] ]
-- if we have taken the partial product up to (1-t^n), then we know that the first n coefficients are now correct
