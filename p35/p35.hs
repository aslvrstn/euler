import Euler

main = print $ length $ filter (\n -> all (isPrime.read) $ rots $ show n) [1..999999]

rots l = map (\x -> (drop x l ++ take x l)) [0..(length l)-1]
