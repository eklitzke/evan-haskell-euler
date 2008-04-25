-- p_adicanalysis.hs

module P_adicAnalysis where

import MathsPrimitives (FunctionRep (..), partialProducts, ($+), ($.) )
import QQ
import UPoly
import CombinatoricsCounting
import NonArchimedean
import PowerSeries
import UmbralCalculus

-- Sources:
-- Robert, A Course in p-adic Analysis


-- MAHLER SERIES

-- Robert, A Course in p-adic Analysis, p161
binomialPoly n = (1 / fromInteger (factorial n)) */ fallingFactorial x n
-- then for x <- NN, evalUP (binomialPoly n) x == x `choose` n
-- the binomial polynomials form a basis for the space of integer-valued functions on NN


dGrad f n = f (n+1) - f n

-- discrete gradient operator, returns f(x+1) - f(x)
dGradUP f = composeUP f (x+1) - f  -- translateUP 1 f - f
-- (also called the finite difference operator)

dGradUP' f = applyCompOp (expPS - 1) f

-- Robert p162
-- express the polynomial as a sum of binomialPolys
-- use the discrete gradient operator to find the coefficients
mahlerSeriesUP' 0 = []
mahlerSeriesUP' f = evalUP f 0 : mahlerSeriesUP (dGradUP f)

-- Robert p214 - the Mahler series of x^n is [S2(n,k)*k! | k <- [0..] ], where S2(n,k) are the Stirling numbers of the second kind
mahlerSeriesUP (UP as) = foldl ($+) [] summands
	where
		summands = zipWith (\a cs -> map (a*) cs) as (map (map fromInteger) powers)
		powers = zipWith (zipWith (*)) stirlingSecondTriangle (repeat (partialProducts (1:[1..])))
-- (faster)

polyFromMahlerSeries ms = sum (zipWith (*/) ms (map binomialPoly [0..]))

-- indefinite sum operator, Robert p167
iSum f 0 = 0
iSum f n = sum [f i | i <- [0..n-1] ]

iSumUP'' f = polyFromMahlerSeries (0 : (mahlerSeriesUP f))
-- This works because iSum of the kth binomial poly is the (k+1)th binomial poly
-- ie iSum (evalUP (binomialPoly k) . fromInteger) n == (evalUP (binomialPoly (k+1)) . fromInteger) n
-- So for example we can check that iSum (evalUP (x^2) . fromInteger) 5 == evalUP (iSumUP (x^2)) 5 == 1^2 + 2^2 + 3^2 + 4^2 == 30

iSumUP' (UP as) = sum (zipWith (*/) as bernoulliSums)

iSumUP f = inverseDeltaOp (expPS - 1) f
-- iSum is inverse of dGrad
-- dGrad (iSum f) n == iSum f (n+1) - iSum f n = f n

bernoulliSums = zipWith (\m p -> (1 /(fromInteger m+1)) */ p) [0..] (tail (zipWith (\bp bn -> bp - UP [bn]) bernoulliPolys bernoulliNumbers))
-- Ireland, Rosen, p231
-- S_m(n) = sum [i^m | i <- [0..n-1] ] = 1/(m+1) * (bp_m+1(n) - bn_m+1)



-- VOLKENBORN INTEGRAL

volkenbornIntegralPoly1 p f = untilConvergence (map (volkenbornSumPoly p f) [1..])

volkenbornSumPoly p f n =
	let Qp _ d as = evalUP (uptoQp p (iSumUP f)) (toQp' p (p^n))
	in Qp p d (drop n as)

-- !! this doesn't work for n>16 because the normalQp call fails. Not quite sure why
volkenbornSumPoly' p f n =
	let Just s = normalQp (Qp p (-n) (1 : repeat 0) * (evalUP (uptoQp p (iSumUP f)) (toQp' p (p^n))))
	in s



-- Robert p265
-- Calculate the Volkenborn integral of a polynomial via its Mahler series (as a sum of binomial polys)
volkenbornIntegralPoly2 :: UPoly QQ -> QQ
volkenbornIntegralPoly2 f = volkenbornIntegralMahlerSeries (mahlerSeriesUP f)
--	let cs = toBinomialBasis f
--	in alternatingSum (zipWith (/) cs (map fromInteger [1..])) -- sum (-1)^k c_k / (k+1)

volkenbornIntegralMahlerSeries ms = alternatingSum (zipWith (/) ms (map fromInteger [1..])) -- sum (-1)^k m_k / (k+1)

alternatingSum xs = foldr (-) 0 xs

-- Robert p270
-- We can work out the Volkenborn integral of polynomials directly, as the Volkenborn integral of a power x^n is the nth Bernoulli number
volkenbornIntegralPoly :: UPoly QQ -> QQ
volkenbornIntegralPoly (UP as) = as $. bernoulliNumbers