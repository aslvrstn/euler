main = print $ prod $ head $ filter pyth $ concat $ map (\x -> (map (\y -> (x,y,1000-x-y))) [x..1000]) [1..1000]

prod (a,b,c) = a*b*c

pyth (a,b,c) = a^2 + b^2 == c^2
