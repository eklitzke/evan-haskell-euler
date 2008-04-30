module Main where

data Fraction = Frac Integer Integer

fracRecip :: Fraction -> Fraction
fracRecip (Frac n d) = Frac d n

fracInc :: Fraction -> Fraction
fracInc (Frac n d) = Frac (n + d) d

magic :: Fraction -> Bool
magic (Frac n d) = (length $ show n) > (length $ show d)

nextFrac :: Fraction -> Fraction
nextFrac f = fracInc (fracRecip (fracInc f))

fracs :: [Fraction]
fracs = take 1000 ((Frac 3 2) : [nextFrac f | f <- fracs])

main = print $ length $ filter magic fracs
