spell_number :: Int -> String
spell_number n
	| n == 1000 = "one thousand"
	| n `mod` 100 == 0 = spell_number (n `div` 100) ++ " hundred"
	| n > 100 = spell_number (n `div` 100) ++ " hundred and " ++ spell_number (n `mod` 100)
	| n == 90 = "ninety"
	| n == 80 = "eighty"
	| n == 70 = "seventy"
	| n == 60 = "sixty"
	| n == 50 = "fifty"
	| n == 40 = "forty"
	| n == 30 = "thirty"
	| n == 20 = "twenty"
	| n == 19 = "nineteen"
	| n == 18 = "eighteen"
	| n == 17 = "seventeen"
	| n == 16 = "sixteen"
	| n == 15 = "fifteen"
	| n == 14 = "fourteen"
	| n == 13 = "thirteen"
	| n == 12 = "twelve"
	| n == 11 = "eleven"
	| n == 10 = "ten"
	| n == 9 = "nine"
	| n == 8 = "eight"
	| n == 7 = "seven"
	| n == 6 = "six"
	| n == 5 = "five"
	| n == 4 = "four"
	| n == 3 = "three"
	| n == 2 = "two"
	| n == 1 = "one"
	| n > 10 = spell_number ((n `div` 10) * 10) ++ spell_number (n `mod` 10)

answer = [x | x <- spelled_out, x /= ' ']
	where
	spelled_out = concat $ map spell_number [1..1000]

main = putStrLn $ show $ length answer
