module Main where

import Data.Char
import Data.Set hiding (map)
import Foreign

triangles = fromList [(n * (n + 1)) `div` 2 | n <- [1..500]]

ordOffset = (ord 'A') - 1

score :: String -> Int
score s = sum [(ord c) - ordOffset | c <- s]

wordsTxt :: [String]
wordsTxt = read ("[" ++ (unsafePerformIO (readFile "words.txt")) ++ "]")

wordScores = map score wordsTxt
answer = length [w | w <- wordScores, w `member` triangles]


main = print answer
