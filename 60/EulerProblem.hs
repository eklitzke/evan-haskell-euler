import Primes (isTrialDivisionPrime)

primes :: [Int]
primes = sieve [2..]
	where sieve (p:ns) = p : sieve (filter (notdiv p) ns)
	      notdiv p n = n `mod` p /= 0

concatInts :: Int -> Int -> Int
concatInts x y = read (show x ++ show y)

concatPairs :: [Int] -> [Int]
concatPairs [x]    = []
concatPairs (x:xs) = [concatInts x y | y <- xs] ++ [concatInts y x | y <- xs] ++ (concatPairs xs)

hasProp :: [Int] -> Bool
hasProp = (all isTrialDivisionPrime) . concatPairs

smallSums :: Int -> Int -> [[Int]]
smallSums n t = filter (/= []) (ss n t primes)
    where
    ss 1 targ (x:xs)
        | x < targ  = ss 1 targ xs
        | x == targ = [[x]]
        | otherwise = [[]]
    ss n targ (x:xs)
        | x > targ  = [[]]
        | otherwise = (myMap (x:) (ss n' t' xs)) ++ (ss n targ xs)
        where
        t' = targ - x
        n' = n - 1
        myMap f ls = map f [y | y <- ls, length y == n']

main = print $ sum $ head $ filter hasProp $ concatMap (smallSums 5) [2..]
--main = print $ [(x, y) | x <- [2..100], let y = smallSums 3 x, y /= []]
