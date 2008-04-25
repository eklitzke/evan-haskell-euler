module Main where

magic :: Integral a => a -> a
magic n = n^n

main = putStrLn $ reverse $ take 10 $ reverse $ show $ sum $ map magic [1..1000]
