main = print $ filter ((>400).length) $ map divisors tris

tris = 0 : (map (\n -> tris!!(n-1)+n) [1..])

divisors n = filter ((==0).(mod n)) [1..n]
