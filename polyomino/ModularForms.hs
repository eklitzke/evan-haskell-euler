-- modularforms.hs

module ModularForms where

import MathsPrimitives (deriv, integ, partialSums, partialProducts)
import ArithmeticFunctions
import CombinatoricsCounting (bernoulliNumber)
import QQ
import PowerSeries


-- EISENSTEIN SERIES

-- Eisenstein series as power series in q = e^(2*pi*i*z)
eisensteinE k | even k = PS (1 : [multiplier * fromInteger (sigma (k-1) n) | n <- [1..]])
	where multiplier = fromInteger (-2 * toInteger k) / bernoulliNumber k  -- -2k/b_k
-- (because it is a series in q, not z, we can't directly test the modular transformation rules)


-- Where we have a sequence of power series which are converging, one coefficient at a time, to a limit
-- We can construct the limit as the diagonal power series of the sequence
diagonalPS fs = PS (zipWith (\(PS as) i -> as !! i) fs [0..])


-- MODULAR DISCRIMINANT (DELTA)

-- the delta function is defined as delta(z) = g2(z)^3 - 27 g3(z)^2 = ((2*pi)^12 / 1728) (E4(z)^3 - E6(z)^2)
-- we omit the (2*pi)^12 factor - delta as power series in q
delta = 1/1728 * ( (eisensteinE 4)^3 - (eisensteinE 6)^2 )


-- Dedekind eta function == q^1/24 * product [1-q^n | n <- [1..] ]
-- we omit the q^1/24 term
dedekindEta = 
	let iterates = partialProducts [(1-t^n) | n <- [1..] ]
	in diagonalPS (1 : iterates)

-- Koblitz p122
-- Alternative expression for delta' is dedekindEta^24 == q * product [(1-q^n)^24 | n <- [1..] ]
delta' = t * dedekindEta ^ 24

-- j function, but without the 1/q term
j = let j' = 1728 * t * (eisensteinE 4)^3 / ( (eisensteinE 4)^3 - (eisensteinE 6)^2 )
    in (j' - 1) / t


-- RAMANUJAN TAU FUNCTION

-- delta/(2*pi)^12 is an OGF for the Ramanujan tau function

ramanujanTaus = fromOGF delta  -- ignore the first coeff - tau(0) not defined

ramanujanTau n | n > 0 = (fromOGF delta) !! n


-- PARTITION FUNCTION

-- Apostol p94
-- sum p(n) t^n = product [1/(1-t^n) | n <- [1..] ]
partitionOGF = 
	let iterates = partialProducts [1/(1-t^n) | n <- [1..] ]
	in diagonalPS (0 : iterates)

-- Note that this is almost the reciprocal of the Dedekind eta function, without the q^1/24 term. Hence, a faster way to calculate it is:

partitionOGF' = (t / delta) ^% (1/24) - 1


-- LAMBERT SERIES

-- See http://en.wikipedia.org/wiki/Lambert_series

-- Lambert Series
-- sum [a_n * q^n / (1-q^n) | n <- [1..] ] == sum [b_n * q^n | n <- [1..] ], where b_n = sum [a_d | d <- [1..n], d `divides` n]


-- express the Lambert series with coefficients a1,a2,... as a power series
-- note: Lambert series start from a1, not a0
lambertSeries as =
	let iterates = partialSums (zipWith (*) as [t^n / (1 - t^n) | n <- [1..] ])
	in diagonalPS (0 : iterates)


-- Of particular interest is:
-- sum [n^a * q^n / (1-q^n) | n <- [1..] ] == sum [sigma a n * q^n | n <- [1..] ]
-- So this gives us another way to calculate the Eisenstein series

-- See http://en.wikipedia.org/wiki/Eisenstein_series


-- Eisenstein series as Lambert series
eisensteinE' k | even k = 1 + multiplier * lambertSeries [fromInteger (n^(k-1)) | n <- [1..] ]
	where multiplier = PS [fromInteger (-2 * toInteger k) / bernoulliNumber k]  -- -2k/b_k

-- Note that in the wikipedia page for Eisenstein series, eisensteinE' 2, 4, 6 are referred to as L(q), M(q), N(q) respectively


-- WEIERSTRASS P FUNCTION

-- Experimental!!

-- p'(z)^2 == 4 p(z)^3 - g2 p(z) - g3 
-- Put w = p(z)
-- => (dw/dz)^2 = 4 w^3 - g2 w - g3
-- => z == integ dw / sqrt (4 w^3 - g2 w - g3)
-- => w = p(z) = inverse of the above integral

weierstrassP g2 g3 = inversePS (integ (1 / sqrt (4 * t^3 - g2 * t - g3)))
-- !! Only works if g3 == -1
-- !! We could do better by extending PowerSeries.sqrt to cope with a0 being a perfect square
-- !! (Or even further, by using PowerSeries over QQ[i], with sqrt defined appropriately)

