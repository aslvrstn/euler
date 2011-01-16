main = print $ foo [1..100]

foo nums = (sq $ sum nums) - (sum $ map (sq) nums)

sq n = n*n
