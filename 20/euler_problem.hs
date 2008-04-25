module Main where
import EulerMath

answer = sum_digits (factorial 100)
main = putStrLn $ show answer
