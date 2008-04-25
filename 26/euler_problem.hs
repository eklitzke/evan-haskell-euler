module Main where
import Data.List

divide_step :: Int -> Int -> Int
divide_step n d = 10 * (d `mod` n)

-- Get the length of a cycle. For example, get_cycle_length [1,2,3,4,5,3] -> 3
-- because [3,4,5] is identified as a cycle
get_cycle_length :: Eq a => [a] -> Int
get_cycle_length xs = i + 1
	where
	Just i = elemIndex (last xs) (tail (reverse xs))

-- Find the length of the recurring part in the reciprocal
recip_len :: Int -> Int
recip_len n = find_div_cycle 1 n []
	where
	find_div_cycle :: Int -> Int -> [Int] -> Int
	find_div_cycle d n seen = let d' = divide_step n d in case d' of
		0         -> 0
		otherwise -> if d' `elem` seen then get_cycle_length seen' else find_div_cycle d' n seen'
			where
			seen' = seen ++ [d']

-- Find the reciprocal with the longest recurring cycle
answer = fst $ maximumBy (\x y -> compare (snd x) (snd y)) [(x, recip_len x) | x <- [1..1000]]

main = putStrLn $ show answer
