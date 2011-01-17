main = print $ foo [1..100]

foo nums = ((sum nums) ^ 2) - (sum $ map (^2) nums)
