module Main where

import Data.Char
import Data.Set hiding (map)
import Foreign
import Control.Monad

triangles = fromList [(n * (n + 1)) `div` 2 | n <- [1..500]]

ordOffset = (ord 'A') - 1

score :: String -> Int
score s = sum [(ord c) - ordOffset | c <- s]

wordsTxt :: IO [String]
wordsTxt = do words <- readFile "words.txt"
              let words' = "[" ++ words ++ "]"
              return $ read words'

wordScores :: IO [Int]
wordScores = do words <- wordsTxt
                return $ map score words

answer :: IO Int
answer = do scores <- wordScores
            return $ length [w | w <- scores, w `member` triangles]

main = do a <- answer
          print a
