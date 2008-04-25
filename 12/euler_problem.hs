module Main where
import EulerMath

main = putStrLn $ show $ head $ filter (\x -> (length (divisors x)) > 500) triangle_numbers
