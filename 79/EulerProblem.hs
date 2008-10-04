import System.IO.Unsafe
import Control.Monad

splitUntilFirst :: Eq a => a -> [a] -> ([a], [a])
splitUntilFirst t xs =
    let a = takeWhile (\x -> x /= t) xs
        b = drop (length a) xs
     in (a, b)

attemptChar :: Char -> String -> String -> (Bool, String)
attemptChar c notAllowed s =
    let (a, b) = splitUntilFirst c s
     in case b of
        [] -> (False, [])
        otherwise -> 
            let bl = (not $ any (\x -> x `elem` notAllowed) a)
             in (bl, b)

attempt :: String -> String -> Bool
attempt pwd truth =
    case pwd of
        (x:xs) ->
            let (b, rem) = attemptChar x xs truth
             in b && (attempt xs rem)
             --in (unsafePerformIO $ print x) `seq` (unsafePerformIO $ print (b, rem)) `seq`(b && attempt xs rem)
        otherwise -> True

possibleAnswer :: Int -> [String] -> Bool
possibleAnswer n attempts =
    let sn = show n
     in all (\x -> attempt x sn) attempts

main = do
    xs <- liftM lines $ readFile "keylog.txt"
    let ls = map init xs
        pa x = possibleAnswer x ls
    print $ head [x | x <- [100..], pa x]
