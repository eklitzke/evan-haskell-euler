import Control.Monad

parseLine :: String -> [Int]
parseLine s = map read (words s)

solveReverse :: [[Int]] -> Int
solveReverse [[x]] = x
solveReverse (a:b:xs) =
    let a' = [ max x y | x <- init a | y <- tail a ]
        b' = zipWith (+) a' b
     in solveReverse $ b' : xs

main = do
    s <- readFile "triangle.txt"
    let t = reverse $ map parseLine $ lines s
    print $ solveReverse t
