module Main where
import EulerMath

answer = head $ reverse $ take 10001 [x | x <- [1..], factor x == [x]]
main = putStrLn $ show answer
