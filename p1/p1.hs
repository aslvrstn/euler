main = print $ sum $ filter (div3or5) [1..1000]

div3or5 n = (mod n 3) == 0 || (mod n 5) == 0
