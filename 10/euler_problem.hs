module Main where
import EulerMath

-- This is way inefficient but it still runs in under a minute. When I fix
-- is_prime to be fast it will be a bit better (a sieve would be better still)
answer = sum [x | x <- [2..1999999], is_prime x]
main = putStrLn $ show answer
