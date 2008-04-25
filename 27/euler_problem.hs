module Main where
import EulerMath
import Primes
import Data.List

compute_quadratic :: Integral a => a -> a -> a -> a
compute_quadratic a b n = (n * n) + (a * n) + b

qp_length :: Integral a => a -> a -> Int
qp_length a b = length $ takeWhile isPrime [compute_quadratic a b n | n <- [0..]]

mycmp (a, b) (c, d) = compare b d

main = putStrLn $ show $ fst $ maximumBy mycmp [(a * b, qp_length a b) | a <- [-999 .. 999], b <- [2 .. 999], (1 + a + b) > 0, isPrime b]
