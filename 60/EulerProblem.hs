import Prelude hiding (lookup)
import Primes (isTrialDivisionPrime, primesTo100, trialDivision)
import Data.Set (fromDistinctAscList, Set, member, size, fromList)

import Data.HashTable
import Control.Monad

import Data.Bits

import Data.Int

import System.IO.Unsafe

type PrimePairHash = HashTable (Int, Int) Bool

primeList :: [Int]
primeList = primesTo100 ++ filter (trialDivision primesTo100) [101,103..]

myPrimeTest = isTrialDivisionPrime

-- Checks the property that writing the concatenated number ab is prime, also
-- ba is prime.
hasProp :: Int -> Int -> Bool
hasProp a b = (myPrimeTest (read $ sa ++ sb)) && (myPrimeTest (read $ sb ++ sa))
    where (sa, sb) = (show a, show b)

-- Check if a pair of primes have the concatenation property we are
-- looking for (namely, hasProp). The function is memoized with a
-- PrimePairHash. The caller should make sure a, b are ordered.
checkHt :: PrimePairHash -> Int -> Int -> IO Bool
checkHt ht a b = do
    t <- lookup ht (a, b)
    case t of
        Just x  -> return x
        Nothing -> do let x = hasProp a b
                      insert ht (a, b) x
                      return x

-- Like all from the Prelude, shortcuts on the first False encountered
allM :: (a -> IO Bool) -> [a] -> IO Bool
allM _ [] = return True
allM f (x:xs) = do v <- f x
                   case v of
                     True  -> allM f xs
                     False -> return False

-- remember can just do odds or evens based on n, handle 2 separately
smallSums :: PrimePairHash -> Int -> Int -> IO [Int]
smallSums _ _ 2010 = return []
smallSums ht n t = do
    print t
    rslt <- ss n t primeList
    case rslt of
      []     -> smallSums ht n (t + 1)
      (x:xs) -> return x
    where
    ss 1 targ (x:xs)
        | x < targ  = ss 1 targ xs
        | x == targ = return [[x]]
        | otherwise = return []
    ss n targ (x:xs)
        | x >= targ = return []
        | otherwise = foo
        where foo = {-# SCC "memoized_check" #-}
                        do t2 <- ss n targ xs
                           withX <- ss (n - 1) (targ - x) xs
                           f <- filterM (allM $ checkHt ht x) withX
                           let t1 = map (x:) f
                           return $ t1 ++ t2

-- hmm, another hash function might be \(a, b) -> (hashInt a) `xor` (hashInt b)
mkHash :: IO PrimePairHash
mkHash = new (==) (hashInt . (uncurry xor))

main = do ht <- mkHash
          ss <- smallSums ht 5 2000
          print ss
