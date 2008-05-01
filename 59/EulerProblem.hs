module Main where

import Foreign
import Data.Bits
import Data.Char
import Data.List

cipherText :: String
cipherText = map chr $ read $ "[" ++ (unsafePerformIO $ readFile "cipher1.txt") ++ "]"

makePasswords :: String -> [String]
makePasswords s
    | length s >= 3 = (take 3 s) : (makePasswords $ tail s)
    | otherwise = []

makeNPerms :: Int -> [a] -> [[a]]
makeNPerms 0 _  = [[]]
makeNPerms n xs = [x:y | x <- xs, y <- makeNPerms (n-1) xs]

-- all of the possible passwords
passwords :: [String]
passwords = makeNPerms 3 "abcdefghijklmnopqrstuvwxyz"

-- decrypts a chunk of plaintext
decryptChunk :: String -> String -> String
decryptChunk key plain
    | lk > lp   = decryptChunk' (take lp key)
    | otherwise = decryptChunk' key
    where 
    decryptChunk' k = map chr [(ord x) `xor` (ord p) | x <- k | p <- plain]
    lk = length key
    lp = length plain

-- decrypt the cipher text with a string
decrypt :: String -> String -> String
decrypt key "" = ""
decrypt key xs = ciphered ++ (decrypt key (drop (length ciphered) xs))
    where
    ciphered = decryptChunk key xs

-- should rewrite to use foldr
allPreds :: [(a -> Bool)] -> a -> Bool
allPreds [] _ = True
allPreds (p:ps) a = (p a) && (allPreds ps a)

containsWords :: (String, String) -> Bool
containsWords (_, s) = allPreds [(\s -> x `isInfixOf` s) | x <- ["and", "the", "of", "with"]] s

trueKey = fst $ head $ filter containsWords [(p, decrypt p cipherText) | p <- passwords]

main = print $ sum $ map ord $ decrypt trueKey cipherText
