I solve it this way:

import Data.Array
import Data.List
import Data.Ord

cache :: Array Integer Integer
cache = array (1,n) ([(i, f i i) | i <- [1..n]])
	where n = 1000000
	f :: Integer -> Integer -> Integer
	f _ 1 = 1
	f x y = r + 1 + if c < x then cache!c else f x c
	where (q, r) = quotRem y 2
	c = (y + (2 * y + 1) * r) `div` 2

	max' :: Integer -> Integer -> Integer
	max' x y = case (comparing (cache!) x y) of
	GT -> x
	_ -> y

	main :: IO()
	main = do putStr (show((\x -> (x, cache!x)) (foldl1' max' $ indices $! cache)))


	Using GHC-6.6:
	* without pre-compilation 29.66 sec
	* with pre-compilation 4.09 sec
