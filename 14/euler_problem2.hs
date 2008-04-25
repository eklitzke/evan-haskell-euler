module Main where
import Data.List

collatz :: Integral a => a -> a
collatz n = if even n then n `div` 2 else 3 * n + 1

collatz_len :: Int -> Int
collatz_len =
	let
		cl 1 = 0
		cl n = 1 + collatz_len (collatz n)
	in (map cl [0..] !!)

tuple_cmp :: (Int, Int) -> (Int, Int) -> Ordering
tuple_cmp (a, b) (c, d) = compare b d

answer = fst $ maximumBy tuple_cmp (map (\x -> (x, collatz_len x)) [1..999999])
main = putStrLn $ show answer
