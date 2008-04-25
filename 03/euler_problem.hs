module Main where
import EulerMath

answer = maximum $ factor 600851475143
main = putStrLn (show answer)
