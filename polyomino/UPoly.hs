-- upoly.hs

module UPoly (UPoly (..), x, (*/), toUPoly, degUP, lcUP, evalUP,
	quotRemUP, divUP, modUP, monomialUP, gcdUP, extendedEuclidUP, toMonicUP,
	derivUP, integUP, composeUP) where

import MathsPrimitives (FunctionRep (..), ($+), ($-) )
import QQ

newtype UPoly a = UP [a] deriving (Eq)
-- the list [a_0, a_1, ..., a_n] represents the polynomial a_0 + a_1 x + ... + a_n x^n

x = UP [0,1] :: UPoly QQ

infixl 7  */, $*


-- ORD INSTANCE

instance (Ord a) => Ord (UPoly a) where
	compare (UP []) (UP [])    = EQ
	compare (UP []) (UP (_:_)) = LT
	compare (UP (_:_)) (UP []) = GT
	compare (UP as) (UP bs) = case compare (length as) (length bs) of
		GT -> GT
		LT -> LT
		EQ -> compare (reverse as) (reverse bs)


-- SHOW INSTANCE

instance (Num a, Show a) => Show (UPoly a) where
	show (UP as) =
		let powers = reverse (filter (\(a,i) -> a /= 0) (zip as [0..]))
		in
			if null powers
			then "0"
			else foldl1 linkTerms (map showTerm powers)
		where linkTerms t u = if head u == '-' then t ++ u else t ++ "+" ++ u

showTerm (a,0) = show a
showTerm (a,i) = showCoeff a ++ showPower i 

showCoeff a = let a' = show a in case a' of
	"1" -> ""
	"-1" -> "-"
	_ -> a'

showPower i
	| i == 0    = ""
	| i == 1    = "x"
	| otherwise = "x^" ++ show i


-- NUM INSTANCE

instance Num a => Num (UPoly a) where
	UP as + UP bs = toUPoly (as $+ bs)
	UP as - UP bs = toUPoly (as $- bs)
	negate (UP as) = UP (map negate as)
	UP as * UP bs = toUPoly (as $* bs)

	fromInteger 0 = UP []
	fromInteger a = UP [fromInteger a]

	signum f = fromInteger 1

toUPoly as = UP (reverse (dropWhile (== 0) (reverse as)))


-- the dollar versions of the arithmetic functions don't remove trailing zeroes
-- when carrying out a sequence of calculations, it is often more efficient to use the dollar versions and remove trailing zeroes at the end

(a:as) $* (b:bs) = [a*b] $+ shiftUP (map (a*) bs $+ map (*b) as) $+ shiftUP (shiftUP (as $* bs))
[] $* _ = []
_ $* [] = []

shiftUP [] = []
shiftUP as = 0 : as

k */ UP as = UP (map (k*) as)


-- OTHER FUNCTIONS

degUP (UP as) = length as - 1

-- leading coefficient
lcUP (UP []) = 0
lcUP (UP as) = last as

evalUP (UP as) v = doEvalUP as
	where
		doEvalUP [] = 0                        -- there's an implicit "fromInteger" call here
		doEvalUP (a:[]) = a                    -- this means that evalUP will work even when the Num instance we're working over doesn't define "fromInteger"
		doEvalUP (a:as) = a + v * doEvalUP as
-- can probably be written more efficiently

derivUP (UP []) = UP []
derivUP (UP as) = UP (zipWith (*) (map fromInteger [1..]) (tail as))

integUP (UP []) = UP []
integUP (UP as) = UP (0 : zipWith (/) as (map fromInteger [1..]))

liftUP (UP as) = UP (map (\a -> UP [a]) as)

composeUP f g = evalUP (liftUP f) g

-- translateUP c f = evalUP (liftUP f) (x+c)


instance (Num a, Fractional a) => FunctionRep (UPoly a) where
	compose = composeUP
	deriv = derivUP
	integ = integUP


quotRemUP _ 0 = error "quotRemUP: division by zero"
quotRemUP f g = doqr g 0 f
	where
		doqr g q r =
			if degUP r < degUP g
			then (q,r)
			else
				let s = monomialUP ((lcUP r) / (lcUP g)) (degUP r - degUP g)
				in doqr g (q + s) (r - s * g)

divUP f g = q where (q,r) = quotRemUP f g

modUP f g = r where (q,r) = quotRemUP f g

monomialUP a n
	| a == 0     = UP []
	| otherwise  = UP (replicate n 0 ++ [a])

gcdUP f g
	| degUP f > degUP g = doGcdUP f g
	| otherwise         = doGcdUP g f
	where
		doGcdUP f 0 = toMonicUP f
		doGcdUP f g = let (q,r) = quotRemUP f g in doGcdUP g r

extendedEuclidUP a b = doExtendedEuclid a b []
	where
		doExtendedEuclid d 0 qs = let (u,v) = unwind 1 0 qs in (u,v,d)
		doExtendedEuclid a b qs = let (q,r) = quotRemUP a b in doExtendedEuclid b r (q:qs)
		unwind u v [] = (u,v)
		unwind u v (q:qs) = unwind v (u-v*q) qs



toMonicUP f = (1/lcUP f) */ f


{-
-- CLASS EUCLIDEAN

class EuclideanDomain a where
	divMod' :: a -> a -> (a,a)
	div' :: a -> a -> a
	mod' :: a -> a -> a

-- gcd' :: EuclideanDomain a => a -> a -> a


-- "INTEGRAL INSTANCE"

-- Unfortunately, the Prelude makes a mess of this
-- To define div, mod etc, you need to be instance Integral
-- But this also commits you to defining toInteger, and it requires context of Real and Enum, requiring you to define toRational, toEnum, and fromEnum

instance Num a => Enum (UPoly a) where
	toEnum n = fromInteger (toInteger n)
	fromEnum _ = error "UPoly.fromEnum: undefined"

instance (Num a, Ord a) => Real (UPoly a) where
	toRational _ = error "UPoly.toRational: undefined"

instance (Num a, Ord a, Fractional a) => Integral (UPoly a) where
	quotRem _ (UP _ []) = error "UPoly.quotRem: division by zero"
	quotRem f g = doqr g 0 f
		where
			doqr g q r =
				if degUP r < degUP g
				then (q,r)
				else
					let s = monomialUP ((lcUP r) / (lcUP g)) (degUP r - degUP g)
					in doqr g (q + s) (r - s * g)



gcd' f (UP _ []) = f
gcd' f g = gcd' g (f `mod` g)



-}

