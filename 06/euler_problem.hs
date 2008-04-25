square x = x * x
answer = square (sum [1..100]) - sum [square x | x <- [1..100]]
main = putStrLn $ show answer
