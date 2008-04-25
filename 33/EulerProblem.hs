module Main where

-- imports factor
import EulerMath
import Data.Set hiding (map)
import Data.Char

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst t (x:xs) = if x == t then xs else x : (removeFirst t xs)

multisetDifference :: Eq a => [a] -> [a] -> [a]
multisetDifference as [] = as
multisetDifference []  _ = []
multisetDifference (a:as) bs
    | a `elem` bs = multisetDifference as (removeFirst a bs)
    | otherwise   = a : (multisetDifference as bs)

-- reduces a fraction. for example, reduceFrac 2 4 = (1, 2)
--reduceFrac :: Integer -> Integer -> (Integer, Integer)
reduceFrac a b = (product as', product bs')
    where
    as  = factor a
    bs  = factor b
    as' = multisetDifference as bs
    bs' = multisetDifference bs as

sharedDigits :: Integral a => a -> a -> Set Char
sharedDigits a b = (fromList (show a)) `intersection` (fromList (show b))

--takeOutDigit :: Integral a => Int -> a -> a
takeOutDigit d n = read (removeOnce d (show n))
    where
    removeOnce _ [] = []
    removeOnce a (b:bs) = if a == b then bs else b : (removeOnce a bs)

--reduceFrac' :: Integral a => a -> a -> (a, a)
reduceFrac' a b = (f a, f b)
    where
    f = takeOutDigit d
    d = head $ toList (sharedDigits a b)

myTest a b = share_digits && nbd10
    where
    share_digits = sharedDigits a b /= empty
    nbd10 = (a `mod` 10 /= 0) || (b `mod` 10 /= 0)

notTrivial a b = not (((div10 a) && (div10 b)) || (((div11 a) && (div11 b))))
    where
    div10 x = (x `mod` 10) == 0
    div11 x = (x `mod` 11) == 0

rf (a, b) = reduceFrac a b

fracs = map rf ( (49, 98) : [(a, b) | a <- [10..98], b <- [a+1 .. 99], myTest a b, (reduceFrac a b) == (reduceFrac' a b)])
fracn = product [a | (a, b) <- fracs]
fracd = product [b | (a, b) <- fracs]
answer = snd (reduceFrac fracn fracd)
main = print answer
