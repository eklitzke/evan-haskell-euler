module Main where

import Data.Set hiding (map, filter)

toFloat :: Integral a => a -> Float
toFloat = fromIntegral

floatQuadratic :: Int -> Int -> Int -> [Float]
floatQuadratic a b c = filter (not . isNaN) [s1, s2]
    where
    a' = toFloat a
    b' = toFloat b
    c' = toFloat c
    rad = sqrt (b'*b' - 4*a'*c')
    (s1, s2) = ((-b' + rad) / (2*a'), (-b' - rad) / (2*a'))

floatQuadratic_ :: Float -> Float -> Float -> [Float]
floatQuadratic_ a b c = filter (not . isNaN) [s1, s2]
    where
    rad = sqrt (b*b - 4*a*c)
    (s1, s2) = ((-b + rad) / (2*a), (-b - rad) / (2*a))

triangles :: [Int]
triangles = [(n * (n+1)) `div` 2 | n <- [286..]]

intQuadratic :: Int -> Int -> Int -> [Int]
intQuadratic a b c = [rs | s <- (floatQuadratic a b c), let rs = round s, a*rs*rs + b*rs + c == 0]

intQuadratic_ :: Float -> Float -> Float -> [Int]
intQuadratic_ a b c = [rs | s <- (floatQuadratic_ a b c), let rs = round s, let rs' = toFloat rs, abs (a*rs'*rs' + b*rs' + c) < 0.01, rs > 0]

isPentagonal :: Int -> Bool
isPentagonal n = (intQuadratic_ 1.5 (-0.5) ((-1.0) * (toFloat n))) /= []

isHexagonal :: Int -> Bool
isHexagonal n = (intQuadratic_ 2.0 (-1.0) (toFloat (-n))) /= []

main = print $ head [t | t <- triangles, isPentagonal t, isHexagonal t]
