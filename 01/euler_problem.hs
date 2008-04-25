number_list = [x | x <- [1..999], or [x `mod` 3 == 0, x `mod` 5 == 0]]
answer = sum number_list

main = putStrLn (show answer)
