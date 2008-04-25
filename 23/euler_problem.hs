module Main where
import EulerMath
import Data.Set

abundants :: [Int]
abundants = [x | x <- [2..28123], abundant_p x]

abundant_combinations :: Set Int
abundant_combinations = fromList [xy | x <- abundants, y <- abundants, y >= x, let xy = x + y, xy <= 28123]

answer :: Int
answer = 1 + sum [x | x <- [2..28123], x `notMember` abundant_combinations]

main = putStrLn $ show answer
