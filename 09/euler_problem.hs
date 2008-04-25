module Main where

abc_triples = [[a, b, c] | a <- [1..998], b <- [a..999], let c = 1000 - a - b, a*a + b*b == c*c]
main = putStrLn $ show $ product $ head abc_triples
