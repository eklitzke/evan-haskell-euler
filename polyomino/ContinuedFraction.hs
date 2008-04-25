-- continuedfraction.hs

module ContinuedFraction where

import NumberTheoryFundamentals (intSqrt)
import QQ
import QuadraticField

-- initially we're only interested in continued fractions for quadratic irrationalities

-- we are looking to write
-- sqrt n = a0 + 1/a1+ 1/a2+ 1/a3+ ...
-- and b0/c0 = a0/1, b1/c1 = (a0a1+1)/a1, etc, ie bi/ci is the value of the truncated continued fraction


-- Our fundamental iteration is that if at stage i, we have
-- sqrt n - bi/ci = z = x + y sqrt n
-- Then we set
-- a_i+1 = floor z

-- the following involves integer arithmetic only, so is exact (no rounding errors)
floorQF (QF n (Q a b) (Q c d)) | a >= 0 && c >= 0 = (a * d + intSqrt (b*b * c*c * n)) `div` (b*d)

-- proof that this is correct.
-- let sqrt (b*b * c*c * n) = intSqrt (b*b * c*c * n) + epsilon, where epsilon < 1
-- then a/b + c/d sqrt n == (a * d + sqrt (b*b * c*c * n)) / (b*d)
--                       == (a * d + intSqrt (b*b * c*c * n)) `div` (b*d)
--                        + ( intSqrt (b*b * c*c * n) `mod` (b*d) + epsilon) / (b*d)
-- The first summand is an integer, so we just need to show that the second summand is < 1
-- Well, this is clear, since anything `mod` (b*d) <= b*d-1, and epsilon < 1, so their sum divided by b*d is < 1

-- !! However, note that it doesn't work if a or c is <0


nextConvergent n ( (bi_2,ci_2), (bi_1,ci_1), ai_1, xi_1) =
	let
		ai = floorQF (QF n 1 0 / xi_1)
		xi = (QF n 1 0 / xi_1) - QF n (Q ai 1) 0
		bi = ai * bi_1 + bi_2
		ci = ai * ci_1 + ci_2
	in ( (bi_1,ci_1), (bi,ci), ai, xi)

convergentsForSqrt n
	| a0*a0 == n  = error ("convergentsForSqrt: " ++ show n ++ " is perfect square")
	| otherwise   = map (\(_,bc,a,_) -> (a,bc)) (iterate (nextConvergent n) ( (1,0),(a0,1),a0,x0 ))
	where
		a0 = intSqrt n
		x0 = QF n (Q (-a0) 1) 1
-- to start the iteration we set b_-1 == 1, c_-1 == 0



-- TEST CODE

toDoubleQF (QF n (Q a b) (Q c d)) = fromInteger a / fromInteger b + sqrt (fromInteger n) * fromInteger c / fromInteger d :: Double

floorQF' z = floor (toDoubleQF z)


-- the version below, which doesn't use quadratic fields directly, succumbs to rounding errors much sooner

nextConvergent' ( (bi_2,ci_2), (bi_1,ci_1), ai_1, xi_1) =
	let
		-- Q ai 1 = floorQF (1/xi_1)
		ai = floor (1/xi_1)
		xi = (1/xi_1) - fromInteger ai
		bi = ai * bi_1 + bi_2
		ci = ai * ci_1 + ci_2
	in ( (bi_1,ci_1), (bi,ci), ai, xi)

convergentsForSqrt' n =
	let
		a0 = intSqrt n
		-- x0 = QF n (-a0/1) 1
		x0 = sqrt (fromInteger n) - (fromInteger a0) 
	in map (\(_,bc,a,_) -> (a,bc)) (iterate nextConvergent' ( (1,0),(a0,1),a0,x0 ))
	-- ie b_-1 == 1, c_-1 == 0

