module Main where

data Fraction = Frac Integer Integer

fracRecip :: Fraction -> Fraction
fracRecip (Frac n d) = Frac d n

fracInc :: Fraction -> Fraction
fracInc (Frac n d) = Frac (n + d) d

nextFrac :: Fraction -> Fraction
nextFrac = fracInc . fracRecip . fracInc

fracs :: [Fraction]
fracs = ((Frac 3 2) : [nextFrac f | f <- fracs])

magic :: Fraction -> Bool
magic (Frac n d) = (length $ show n) > (length $ show d)

main = print $ length $ filter magic $ take 1000 fracs
