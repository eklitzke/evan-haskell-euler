module Main where
import EulerMath

main = putStrLn $ show $ head [a | (a, b) <- zip [1..] fibs, num_digits b >= 1000]
