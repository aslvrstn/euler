main = print $ 0

rots l = map (\x -> (drop x l ++ take x l)) [0..(length l)-1]
