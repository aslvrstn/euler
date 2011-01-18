main = print $ filter ((>500).length) $ map divisors [1..]

divisors n = filter (\x -> mod n x == 0) [1..n]
