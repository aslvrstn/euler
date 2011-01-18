main = print $ filter ((>500).length) $ map divisors [1..]

divisors n = filter ((==0).(mod n)) [1..n]
