module Main where

palindrome :: Eq a => [a] -> Bool
palindrome []  = True
palindrome [_] = True
palindrome (x:xs) = (x == (last xs)) && (palindrome (init xs))

isNumPalindrome :: (Show a, Integral a) => a -> Bool
isNumPalindrome = palindrome . show

reverseNum :: (Show a, Integral a, Read a) => a -> a
reverseNum = read . reverse . show

--reverseAndAdd :: Integral a => a -> a
reverseAndAdd n = n + (reverseNum n)

--lyrchel :: Integral a => a -> Bool
lyrchel n = lyrchel_ 50 n
    where
    lyrchel_ 0 _ = True
    lyrchel_ c n = if isNumPalindrome raa then False else lyrchel_ (c-1) raa
        where
        raa = reverseAndAdd n

main = print $ length [x | x <- [1..9999], lyrchel x]
