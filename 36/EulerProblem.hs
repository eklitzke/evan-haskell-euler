module Main where

import Numeric

palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome [x] = True
palindrome (x:xs) = (x == (last xs)) && palindrome (init xs)

octToBinary :: String -> String
octToBinary "" = []
octToBinary (x:xs) = case x of
    '0' -> build "000"
    '1' -> build "001"
    '2' -> build "010"
    '3' -> build "011"
    '4' -> build "100"
    '5' -> build "101"
    '6' -> build "110"
    '7' -> build "111"
    where
    build s = s ++ (octToBinary xs)

asBinary :: Integral a => a -> String
asBinary n = removeZeroes $ octToBinary $ showOct n ""
    where
    removeZeroes [] = "0"
    removeZeroes ('0':xs) = removeZeroes xs
    removeZeroes xs = xs

test :: Int -> Bool
test n = (palindrome (show n)) && (palindrome (asBinary n))

answer = sum [x | x <- [1..1000000], test x]
main = print answer
