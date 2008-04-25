leap_year_p :: Int -> Bool
leap_year_p y = (y `mod` 4 == 0) && (y `mod` 400 /= 0)

days_in_month_year :: Int -> Int -> Int
days_in_month_year 2 y = if leap_year_p y then 29 else 28
days_in_month_year m _
	| m == 1 = 31
	| m == 3 = 31
	| m == 4 = 30
	| m == 5 = 31
	| m == 6 = 30
	| m == 7 = 31
	| m == 8 = 31
	| m == 9 = 30
	| m == 10 = 31
	| m == 11 = 30
	| m == 12 = 31

find_solution :: Int -> Int -> Int -> Int -> Int
find_solution count day month year
	| year == 2001 = count
	| year == 1900 = find_solution count day' month' year'
	| otherwise = find_solution count' day' month' year'
	where
	count' = count + if day `mod` 7 == 0 then 1 else 0
	day' = day + days_in_month_year month year
	month' = (month `mod` 12) + 1
	year' = year + if month' == 1 then 1 else 0

answer = find_solution 0 1 1 1900
main = putStrLn $ show (answer + 1)
