data Fraction = Fract Int Int

num :: Fraction -> Int
num Frac n d = n

denom :: Fraction -> Int
denom Frac n d = d

-- Remove the first element in a list matching a pattern
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ []     = []
removeFirst t (x:xs) = if x == t then xs else x : (removeFirst t xs)

reduce :: Fraction -> Fraction
reduce Frac n d

--isReduced :: Fraction -> Bool

fracDiv :: Int -> Fraction -> Fraction
fracDiv x (Frac n d) = Frac ( 


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

fracDiv :: 
