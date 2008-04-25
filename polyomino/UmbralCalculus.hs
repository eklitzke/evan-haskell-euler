-- umbralcalculus.hs

module UmbralCalculus where

import MathsPrimitives (FunctionRep (..), partialProducts, factorials, ($+), ($.) )
import QQ
import CombinatoricsCounting
import PowerSeries
import UPoly


-- Sources:
-- Robert, A Course in p-adic Analysis
-- http://en.wikipedia.org ("umbral calculus", "binomial type", "hermite polynomials", "touchard polynomials")


-- COMPOSITION OPERATORS

-- A translation is an operator t_a defined by (t_a f) (x) = f (x+a). (It is a linear operator on the vector space of polynomials)

-- Robert p198-9
-- A composition operator is a linear operator on the vector space of polynomials that commutes with translations
-- The composition operators are precisely the power series in D, the d/dx operator

applyCompOp (PS as) f = sum (doApplyCompOp as f)
	where
		doApplyCompOp [] _ = []
		doApplyCompOp _ 0 = []
		doApplyCompOp (a:as) h =
			let h' = derivUP h
			in (a */ h) : doApplyCompOp as h'


-- DELTA OPERATORS

-- Robert, p197
-- A delta operator is a linear operator d on the the space of polynomials, such that
-- 1. d commutes with all translations t_a (ie, d is a composition operator)
-- 2. d(x) = c, a non-zero constant
-- The delta operators are precisely the power series in D, a0 + a1D + ..., with a0 == 0, a1 /= 0

isDeltaOp (PS (a0:a1:_)) = a0 == 0 && a1 /= 0

inverseDeltaOp delta f = integUP (applyCompOp (t/delta) f)
-- find g such that delta g == f
-- => delta (D g) == D (delta g) == D f
-- => D g == 1/delta (D f) = (D/delta) f
-- => g = integUP ( (D/delta) f ) 

-- the basic sequence of a delta operator is the (unique) sequence of polynomials p_n(x) such that
-- deg p_n = n
-- delta p_n = n p_n-1, n >= 1
-- p_0 = 1; p_n(0) == 0, n >= 1
basicSequence delta | isDeltaOp delta = 1 : doBasicSequence 1 1
	where doBasicSequence n f' =
		let f = inverseDeltaOp delta (fromInteger n */ f')
		in f : doBasicSequence (n+1) f

-- The basic sequence of a delta operator satisfies
-- p_n (x+y) == sum [n `choose` k * p_k(x) * p_n-k(y) | k <- [0..n] ]
-- This follows from taking the generalised Taylor series of p_n wrt delta around x

-- Robert p206, we can calculate the nth poly in the basic sequence directly
basicPoly _ 0 = 1
basicPoly delta@(PS (0:as)) n = x * applyCompOp ((recip (PS as))^n) (x^(n-1)) 


-- SHEFFER SEQUENCES

-- Robert, p208
-- A Sheffer sequence (relative to delta) is a sequence of polynomials s_n such that
-- 1. deg s_n == n
-- 2. delta s_n = n s_n-1, n >= 1

-- The Sheffer sequences are precisely the sequences s_n = S (p_n), where p_n is the basic system of the delta operator d, and S is an invertible composition operator

isInvertible (PS (a0:_)) = a0 /= 0

shefferSequence delta s
	| isInvertible s  = map (applyCompOp s) (basicSequence delta)
	| otherwise       = error "shefferSequence: s is not invertible"

-- A Sheffer sequence wrt the differentiation operator D is called an Appell sequence

appellSequence = shefferSequence t

-- Robert p 208
-- A Sheffer sequence satisfies
-- s_n(x+y) == sum [n `choose` k * p_k(x) * s_n-k(y) | k <- [0..n] ]
-- In particular, an Appell sequence satisfies
-- s_n(x+y) == sum [n `choose` k * x^k * s_n-k(y) | k <- [0..n] ]


-- GENERALISED MACLAURIN SERIES
-- Given a delta operator, with basic sequence p0, p1, ..., and a polynomial f
-- The generalised Maclaurin series is f == f(0) p0 + (delta f)(0) p1(x) + (delta delta f)(0) p2(x) / 2! + ...

-- returns the terms f(0), (delta f)(0), (delta delta f)(0), ...
genMaclaurinDerivsUP _ 0 = []
genMaclaurinDerivsUP delta f = evalUP f 0 : genMaclaurinDerivsUP delta (applyCompOp delta f)

-- returns the terms f(0)/0!, (delta f)(0)/1!, (delta delta f)(0)/2!, etc
genMaclaurinCoeffsUP delta f = zipWith (/) (genMaclaurinDerivsUP delta f) (map fromInteger factorials)

-- So we expect that
-- sum (zipWith (*/) (genMaclaurinCoeffsUP delta f) (basicSequence delta)) == f

-- Examples:
-- genMaclaurinCoeffsUP t (x^2-x) == [0,-1,1] - it just returns the poly

-- from Cameron, Combinatorics, p81
-- (x)_n = sum s(n,k) x^k
-- => genMaclaurinCoeffsUP t (fallingFactorial x (toInteger n)) == map fromInteger (stirlingFirstTriangle !! n)
-- x^n = sum S(n,k) (x)_k
-- => genMaclaurinCoeffsUP (exp t - 1) (x^n) == map fromInteger (stirlingSecondTriangle !! n)

-- We could also do Taylor series, ie evaluate the derivatives at an arbitrary point, rather than zero


-- BERNOULLI POLYNOMIALS

-- Bernoulli polynomials via recurrence relation, Robert p272
bernoulliPolys = b0 : doBernoulliPolys [1,2,1] [b0] 1
	where
		b0 = fromInteger 1 :: UPoly QQ
		doBernoulliPolys cs bs n =
			let
				cs' = nextPascal cs
				b = x^n - (1 / fromInteger (n+1)) */ (cs $. bs)
			in b : doBernoulliPolys cs' (bs++[b]) (n+1)

-- Bernoulli polynomial via explicit expression, Robert p271 
bernoulliPoly' m = sum [fromInteger (m `choose` k) * bernoulliNumber (fromEnum k) */ (x^(m-k)) | k <- [0..m] ]

bernoulliPoly m = applyCompOp bernoulliEGF (x^m)
-- (faster)

bernoulliSS = appellSequence bernoulliEGF


-- HERMITE POLYNOMIALS
-- from http://en.wikipedia.org/wiki/Hermite_polynomials

-- Hermite polys can be defined by the recurrence relation H_n+1 (x) = x H_n(x) - d/dx H_n(x)
hermitePolys = doHermitePolys 1
	where doHermitePolys f = f : doHermitePolys (x * f - derivUP f)

hermitePoly n = hermitePolys !! n

-- the hermite polys can be derived as terminating power series
-- hermite n = (-1)^n * e^(t^2/2) * D^n (e^(-t^2/2))
hermite n = (-1)^n * exp ((t^2)/2) * ((iterate deriv (exp (-(t^2)/2))) !! n)

hermitePS = exp (-(t^2)/2)

-- alternatively, the Hermite polys can be got by applying e^(-(D^2)/2) to the powers x^n
hermitePoly' n = applyCompOp hermitePS (x^n)

hermiteSS = appellSequence hermitePS

-- combinatorial interpretation
-- the coefficient of x^k in H_n(x) is the number of unordered partitions of n into k singletons and (n-k)/2 pairs
-- eg H4(x) = x^4 - 6 x^2 + 3
-- so there are 6 partitions of 4 into 2 singletons and 1 pair, namely
-- [[1,2],[3],[4]], [[1,3],[2],[4]], [[1,4],[2],[3]], [[1],[2,3],[4]], [[1],[2,4],[3]], [[1],[2],[3,4]]


-- TOUCHARD POLYNOMIALS
-- from http://en.wikipedia.org/wiki/Touchard_polynomials

-- (called Bell polynomials by Robert, p211)
-- using the recursion T_n+1(x) = x * sum [(n `choose` k) * T_k(x) | k <- [0..n] ]
touchardPolys = b0 : doTouchardPolys [1] [b0]
	where
		b0 = fromInteger 1 :: UPoly QQ
		doTouchardPolys cs bs =
			let
				cs' = nextPascal cs
				b = x * (cs $. bs)
			in b : doTouchardPolys cs' (b:bs)

-- the Touchard polynomials are the basic sequence for the delta operator log(1+D)
touchardPolys' = basicSequence log1PS

touchardPoly n = UP (map fromInteger (stirlingSecondTriangle !! n))

touchardPoly' n = touchardPolys !! n

-- evalUP (touchardPoly n)  1 == bellNumber n

touchardSS = shefferSequence log1PS 1


-- LAGUERRE POLYNOMIALS


-- modified from Dan's code
laguerreSS nu = shefferSequence (t/(1-t)) ((1-t)^(nu+1))

-- laguerre0 n = e^t / n! * D^n (e^-t * t^n)
laguerre0 0 = 1
laguerre0 n = exp t / fromInteger (product [1..toInteger n]) * ((iterate deriv (exp (-t) * (t^n))) !! n)


laguerre nu n = ((iterate deriv (t^(n+nu) * exp (-t))) !! n) * exp t / (factorial n * t^nu) 
	where factorial n = fromInteger (product [1..toInteger n])


-- !! NOT WORKING - gives wrong answer
-- from recurrence relation, Hassani, Mathematical Physics: A Modern Introduction to its Foundations, p181
laguerrePolys nu = 1 : f1 : doLaguerrePolys (1,f1,1)
	where
		f1 = x - (fromInteger nu + 1)
		doLaguerrePolys (n, f_n, f_n_1) =
			let f = recip (n+1) */ ( ((2*n + fromInteger nu + 1) */ f_n) - (x * f_n) - ((n + fromInteger nu) */ f_n_1) )
			in f : doLaguerrePolys (n+1,f,f_n)


-- VARIANT CODE

-- find g such that delta g == f, by "integrating" termwise
-- note that the following algorithm only works for delta operators, because it requires that deg (delta f) == deg f - 1
inverseDeltaOp' delta f
	| f == 0 = 0
	| isDeltaOp delta =
		let
			n = degUP f
			xn1 = x^(n+1)
			xn1' = applyCompOp delta xn1
			lcg = lcUP f / lcUP xn1'
		in lcg */ xn1 + inverseDeltaOp delta (f - lcg */ xn1')
	| otherwise = error ("inverseDeltaOp: " ++ show delta ++ " is not a delta operator")


-- POWER SERIES IN DIFF OPS

_D f = derivUP f
_xD f = x * derivUP f
_Dx f = derivUP (x*f)
_Dx_xD f = _Dx f - _xD f

-- !! But applying either _xD or _Dx doesn't terminate on polys

applyPS op (PS as) f = sum (doApplyPS as f)
	where
		doApplyPS [] _ = []
		doApplyPS _ 0 = []
		doApplyPS (a:as) h =
			let h' = op h
			in (a */ h) : doApplyPS as h'




