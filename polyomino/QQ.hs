-- qq.hs

module QQ where

-- replacement implementation of rationals
-- preferred to Prelude version because improved show functions

data QQ = Q Integer Integer deriving Eq
-- rationals - Q x y means x/y

instance Show QQ where
	show (Q x y)
		| y == 1     = show x
		| otherwise  = show x ++ "/" ++ show y

toQ x y
	| y > 0   = toQ' x y
	| y < 0   = toQ' (-x) (-y)
	| y == 0  = error "toQ: division by zero"

-- reduce to lowest terms - assumes y > 0
toQ' x y = Q (x `quot` d) (y `quot` d)
	where d = gcd x y

instance Num QQ where
	Q x y + Q x' y' = toQ' (x*y' + x'*y) (y*y')
	negate (Q x y) = Q (negate x) y
	Q x y * Q x' y' = toQ' (x*x') (y*y')
	fromInteger n = Q n 1
	abs (Q x y) = Q (abs x) y
	signum (Q x y) = Q (signum x) 1

instance Fractional QQ where
	recip (Q x y)
		| x > 0   = Q y x
		| x < 0   = Q (-y) (-x)
		| x == 0  = error "QQ.recip: Q 0 _"

instance Ord QQ where
	compare (Q x y) (Q x' y') = compare (x*y') (x'*y)


isIntegerQ (Q x y) = y == 1

isHalfIntegerQ (Q x y) = y == 2  -- in lowest terms, so this implies that x odd

