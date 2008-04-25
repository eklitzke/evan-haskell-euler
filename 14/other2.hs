import Data.Array
import Data.List

syrs n = listArray (1,n) $ 0:[1 + syr n x | x <- [2..n]]
	where
	syr n x = if x' <= n then a ! x' else 1 + syr n x'
		where x' = if even x then x `div` 2 else 3 * x + 1

main = print $ foldl' maxBySnd (0,0) $ assocs $ syrs 1000000
	where maxBySnd x@(_,a) y@(_,b) = if a > b then x else y
