-- dirichletseries.hs

module DirichletSeries where

import NumberTheoryFundamentals (divides)
import MathsPrimitives ( ($+), ($-) )
import QQ
import ArithmeticFunctions
import UPoly


-- Dirichlet series is sum a_n n^-s

data DirichletSeries a = DS [a]

precisionDS = 16 :: Int

coeffsDS (DS as) = as


DS as ~= DS bs = take precisionDS as == take precisionDS bs

instance Eq (DirichletSeries a) where
	_ == _ = error "DirichletSeries.== : not defined, use ~= instead"

instance Show a => Show (DirichletSeries a) where
	show (DS as) = "DS " ++ show (take precisionDS as)



-- NUM INSTANCE

instance Num a => Num (DirichletSeries a) where
	DS as + DS bs = DS (as $+ bs)
	negate (DS as) = DS (map negate as)
	DS as * DS bs = DS (as $* bs)
	fromInteger n = DS (fromInteger n : repeat 0)


-- as $+ bs = zipWith (+) as bs
addDS as bs = as $+ bs

as $* bs = as `multDS'` bs

-- Naive method - slower than the following, but safe
multDS as bs = map coeff [1..]
    where coeff n = sum [(as !!! d) * (bs !!! (n `div` d)) | d <- [1..n], d `divides` n]

xs !!! i = xs !! (i-1)         -- Dirichlet series are indexed from 1, not 0

-- More efficient - but grows heap as more coefficients required, eventually lead to failure
multDS' (a:as) bs = doMult (map (a*) bs) as 1
    where doMult (c:cs) (a:as) i =
              let cs' = cs `addDS` (map (a*) (i `zerosBetween` bs))
              in c : doMult cs' as (i+1)
-- The Dirichlet series for [a1,a2,a3,...] * [b1,b2,b3,...] is
-- map (a1*) [b1,b2,b3,...] + map (a2*) [0,b1,0,b2,0,b3,...] + map (a3*) [0,0,b1,0,0,b2,0,0,b3,...] + ...
-- The idea of the code is to avoid an infinite sum by only adding the i'th summand at the point where we're calculating the i'th coefficient.
-- for example, when i == 1, c:cs = a1*b1 : [a1*b2, a1*b3,...], a:as = a2:[a3,a4,...]
-- so bs' = [b1,0,b2,0,...], cs' = [a1*b2, a1*b3, ...] $+ [a2*b1,0,a2*b2,0,...]
-- so when i == 2, c:cs = a1*b2+a2*b1 : [a1*b3, a1*b4+a2*b2, a1*b5, ...], a:as =a3:[a4,a5,...]
-- so bs' = [b1,0,0,b2,0,0,...], cs' = [a1*b3, a1*b4+a2*b2, a1*b5] $+ [a3*b1,0,0,a3*b2,0,0,...]
-- etc.

k `zerosBetween` (x:xs) = x : replicate k 0 ++ k `zerosBetween` xs


-- FRACTIONAL INSTANCE

instance Fractional a => Fractional (DirichletSeries a) where
	recip (DS as) = DS (recipDS as)


-- Naive method
recipDS' (0:_) = error "recipDS: a1 == 0"
recipDS' as@(a:_) = (1/a) : doRecipDS 2 [1/a]
    where doRecipDS n bs =
              let bn = (-1/a) * sum [(as !!! d) * (bs !!! (n `div` d)) | d <- [2..n], d `divides` n]
              in bn : doRecipDS (n+1) (bs ++ [bn])

-- Much faster method
recipDS (0:_) = error "recipDS: a1 == 0"
recipDS (a:as) = (1/a) : doRecip [1/a] (map ((1/a)*) as) 1
    where doRecip bs (c:cs) n =
              let b_n1 = -c/a
                  cs' = cs `addDS` (replicate n 0 ++ map (b_n1*) (n `zerosBetween` as))
              in b_n1 : doRecip (bs ++ [b_n1]) cs' (n+1)
-- at step n, we have bs = [b_1..b_n], c:cs = c_n+1 : [c_n+2..], where [a1..] * [b1..bn] == [c1..]
-- we want c_n+1 == 0, so we set b_n+1 = - c_n+1 / a1
-- we must then set [c1'..] = [c1..] + b_n+1 (n+1)^-s * [a1..]
-- the thing we're adding in this case is b_n+1 a1 (n+1)^-s + b_n+1 a2 (2n+2)^-s + ... == replicate n 0 ++ map (b_n+1*) (stretch n [a1..])
-- however, in the tail recursion we only need to pass in the [c_n+2..] terms, so dropping the first n+1 terms of the above, we get the code above 


-- LINEAR CHANGE OF VARIABLE
-- Given a Dirichlet series f(s) = sum a_n n^-s,
-- we support linear changes of variable, f(a*s+b), for a,b <- ZZ, a > 0

s = UP [0,1] :: UPoly Integer

-- composeDS (DS as) (UP [b,a]) | a > 0 = DS (translate b (dilate a as))

ofDS (DS as) (UP [b,a]) | a > 0 = DS (translate b (dilate a as))

-- given f(s) = sum a_n n^-s, return f(s+k) = sum a_n n^-k n^-s
translate 0 as = as
translate k as = zipWith (*) as (map (\n -> fromInteger n^^(-k)) [1..])

-- given f(s) = sum a_n n^-s, return f(k*s) == sum a_n n^-ks == sum a_n (n^k)^-s
dilate 1 as = as
dilate k as = doDilate as 1
	where doDilate (a:as) i = a : replicate ((i+1)^k-i^k-1) 0 ++ doDilate as (i+1)

-- given f(s) = sum a_n n^-s, return f(2s) = sum a_n n^-2s
double as = doDouble as 2
	where doDouble (a:as) i = a : replicate i 0 ++ doDouble as (i+2)


-- SOME INTERESTING DIRICHLET SERIES
-- from Hardy and Wright

-- Riemann zeta function
-- zeta(s) = 1/1^s + 1/2^s + 1/3^s +...
zetaDS :: DirichletSeries QQ
zetaDS = DS (repeat 1)

zeta f = zetaDS `ofDS` f

-- series for d n = length [d | d `divides` n]
d_DGF = (zeta s) ^2

-- series for sigma n = sum [d | d `divides` n]
sigmaDGF = zeta s * zeta (s-1)

-- series for sigma_k n = sum [d^k | d `divides` n]
sigma_kDGF k = zeta s * zeta (s-k)

mobiusDGF = 1 / zeta s

-- euler totient function, phi n = length [m | m <- [1..n], coprime m n]
eulerTotientDGF = zeta (s-1) / zeta s

-- dgf for q_2(n) == 0 if n has a square as a factor, 1 otherwise
squarefreeDGF = zeta s / zeta (2*s)

-- dgf for q_k(n) == 0 if n has a kth power as a factor, 1 otherwise
powerfreeDGF k = zeta s / zeta (k*s)


-- EULER PRODUCTS

-- given [(p,f)] for primes p, where f is Dirichlet Series of the form 1 + a_p p^-s + a_2p p^-2s + ...
-- return their product
fromPrimeProductDS pas = DS (1 : doPrimeProduct pas 2 1)
	where
		doPrimeProduct ((p, f):pas) p' series@(DS bs) =
			drop (p'-1) (take (p-1) bs) ++ doPrimeProduct pas p (series*f)
		doPrimeProduct [] p' (DS bs) = drop (p'-1) bs


-- given (p,a_p), return (1 - a_p p^-s)^-1 == 1 + a_p p^-s + a_p^2 p^-2s + ...
eulerTermDS' (p,a_p) = recip (DS (1:replicate (p-2) 0 ++ (-a_p):repeat 0))

-- This is much faster way to achieve the same result
eulerTermDS (p,a_p) = DS (doEulerTerm (1,1))
	where doEulerTerm (q,a_q) = let q' = p*q in a_q : replicate (q'-q-1) 0 ++ doEulerTerm (q',a_p*a_q)


-- given [(p,a_p)] for primes p, return product (1 - a_p p^-s)^-1
fromEulerProductDS pas = fromPrimeProductDS [(p, eulerTermDS (p,a_p)) | (p,a_p) <- pas]


-- OTHER STUFF

deriv_zeta = map (negate . log . fromInteger) [1..]
-- n^-s = e^(-s log n)
-- so deriv n^-s = (- log n) e^(-s log n) = (- log n) n^-s

-- deriv as = zipWith (*) (map fromInteger as) deriv_zeta

-- lambda function: lambda (p^m) = log p, lambda n = 0 otherwise
-- lambda_dgf = map negate (deriv_zeta $* map fromInteger recip_zeta)


-- Hardy and Wright p 254ff has several others

