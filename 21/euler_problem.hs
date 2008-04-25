module Main where
import EulerMath

sum_pd :: Int -> Int
sum_pd = sum . proper_divisors

amicable_p :: Int -> Bool
amicable_p n = (n /= n') && (n == sum_pd n')
	where
	n' = sum_pd n

answer = sum [x | x <- [2..9999], amicable_p x]
main = putStrLn $ show answer
