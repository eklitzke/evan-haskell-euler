module Main where
import EulerMath

candidates = [x * y | x <- [100..999], y <- [1000 - x..1000], y >= 100]
answer = maximum $ filter is_palindrome candidates

main = putStrLn $ show answer
