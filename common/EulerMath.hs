module EulerMath where
import Control.Monad
import Data.List
import Data.Char

golden_ratio :: Double
golden_ratio = (1 + sqrt 5) / 2

{-
fibs :: Integral a => [a]
fibs = 1 : 1 : [x + y | x <- fibs | y <- tail fibs]

-- Returns a list of the Fibonacci numbers less than x
fibs_to :: Int -> [Int]
fibs_to x = take n fibs
    where
    n = floor (log (sqrt 5 * fromIntegral x) / log golden_ratio)
-}

factor_slow :: Integral a => a -> [a]
factor_slow 1 = []
factor_slow n = fac : (factor_slow (n `div` fac))
	where
	fac = test_factors 2 (ceiling $ sqrt $ fromIntegral n)
	test_factors a lim = if a > lim then n else (if n `mod` a == 0 then a else test_factors (a+1) lim)

int_root :: Integral a => a -> a
int_root = floor . sqrt . fromIntegral

square_p :: Integral a => a -> Bool
square_p n = n == ns * ns
	where
	ns = int_root n

factor_fermat :: Integral a => a -> [a]
factor_fermat 1 = []
factor_fermat 2 = [2]
--factor_fermat n = (factor_fermat fac) ++ (factor_fermat fac')
factor_fermat n = if fac == 1 then [n] else (factor_fermat fac) ++ (factor_fermat fac')
	where
	a = ceiling $ sqrt $ fromIntegral n
	fac = fermat a (a*a - n)
	fac' = n `div` fac
	fermat x y = if square_p y then x - (int_root y) else fermat (x + 1) (x * x - n)

factor_rho :: Int -> [Int]
factor_rho n = if fac == n then [n] else (factor_rho fac) ++ (factor_rho (n `div` fac))
    where
    fac = rho_closure 2 2 1 1
    rho_closure x y d c = if d == 1 then rho_closure x' y' d' c else (if d == n then rho_closure 2 2 1 (c + 1) else d)
        where
        x' = f x
        y' = f (f y)
        d' = gcd (abs (x' - y')) n
        f z = (z * z + c) `mod` n

factor :: Integral a => a -> [a]
factor = factor_slow

-- kind of ghetto
all_factors :: Integral a => a -> [[a]]
all_factors n = filter (\x -> product x == n) (powerset (factor n))

-- Is a number a palindrome?
is_palindrome :: Integral a => a -> Bool
is_palindrome n = s == reverse s
    where
    s = show n

triangle_numbers :: Integral a => [a]
triangle_numbers = [(n*n + n) `div` 2 | n <- [1..]]

-- I'm not really this smart, I found this on the internet
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

divisors :: Integral a => a -> [a]
divisors = nub . map product . powerset . factor

proper_divisors :: Integral a => a -> [a]
proper_divisors n = filter (/= n) (divisors n)

perfect_number_p :: Integral a => a -> Bool
perfect_number_p n = n == sum (proper_divisors n)

abundant_p :: Integral a => a -> Bool
abundant_p n = n < sum (proper_divisors n)

factorial :: Integral a => a -> a
factorial n = product [1..n]

nCr :: Integral a => a -> a -> a
nCr n k = factorial n `div` (factorial k * factorial (n - k))

nPr :: Integral a => a -> a -> a
nPr n r = product [r'..n]
	where
	r' = n - r + 1

num_digits :: Integral a => a -> Int
num_digits = length . show

sum_digits :: Integral a => a -> Int
sum_digits = sum . map digitToInt . show

permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = [zs | ys <- permutations xs, zs <- everywhere x ys]

everywhere :: a -> [a] -> [[a]]
everywhere x []     = [[x]]
everywhere x (y:ys) = (x:y:ys) : [y:zs | zs <- everywhere x ys]

pandigital :: String -> Bool
pandigital s = (length s == 9) && (sort s == "123456789")

divides :: Integral a => a -> a -> Bool
a `divides` b = b `mod` a == 0
