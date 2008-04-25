module Main where

import EulerMath
import Data.Maybe

magic :: Integer -> Maybe Integer
magic n = buildMagic 1 ""
    where
    buildMagic s s'
        | ls > 9 =  Nothing
        | ls == 9 = if pandigital s' then Just (read s') else Nothing
        | otherwise = buildMagic (s+1) (s' ++ (show (s * n)))
        where ls = length s'

answer = maximum [fromJust mx | x <- [9..10000], let mx = magic x, isJust mx]

main = print answer
