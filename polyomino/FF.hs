-- ff.hs

module FF where

import NumberTheoryFundamentals (extendedEuclid)
import Bits ( (.&.), xor )

data FF = F Integer Integer -- deriving Show

instance Eq FF where
	F p x == F q y
		| p == q            = x == y
		| p == 0 && q == 0  = False  -- because we already know x /= y
		| p == 0            = x `mod` q == y
		| q == 0            = x == y `mod` p

instance Ord FF where
	compare (F _ x) (F _ y) = compare x y

instance Show FF where
	show (F p x) = show x
--		| p == 0          = show x
--		| x <= p `div` 2  = show x
--		| otherwise       = show (x-p)

toFp p x = F p (x `mod` p)
-- finds the representative from 0..p-1

instance Num FF where
	F 0 x + F 0 y           = F 0 (x+y)
	F p x + F q y | p == q  = toFp p (x+y)
	F p x + F 0 y           = toFp p (x+y)
	F 0 x + F q y           = toFp q (x+y)

	negate (F p 0) = F p 0
	negate (F p x) = F p (p-x)

	F 0 x * F 0 y           = F 0 (x*y)
	F p x * F q y | p == q  = toFp p (x*y)
	F p x * F 0 y           = toFp p (x*y)
	F 0 x * F q y           = toFp q (x*y)

	fromInteger n = F 0 n


instance Fractional FF where
	recip (F 0 _)    = error "FF.recip: F 0 _"
	recip (F p x)
		| x == 0     = error "FF.recip: F p 0"
		| x == 1     = F p 1
		| otherwise  = toFp p u where (u,v,1) = extendedEuclid x p
-- this has consequence that F p x / F 0 y results in error, whereas we could evaluate to F p (x/y)


x % p = toFp p x


-- F2

newtype F2 = F2 Int deriving (Eq, Ord)

instance Show F2 where
	show (F2 x) = show x

instance Num F2 where
	F2 x + F2 y = F2 (x `xor` y)
	negate (F2 x) = F2 x
	F2 x * F2 y = F2 (x .&. y)
	fromInteger n = if even n then F2 0 else F2 1

instance Fractional F2 where
	recip (F2 1) = F2 1
	recip (F2 0) = error "F2.recip: division by zero"
