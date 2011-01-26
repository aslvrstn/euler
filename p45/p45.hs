main = print $ filter (\t -> elemSorted t pents) hexs

-- Every hex is a tri. Every other tri is a hex
tris = map (\n -> n*(n+1) `div` 2) [1..]
pents = map (\n -> n*(3*n-1) `div` 2) [1..]
hexs = map (\n -> n*(2*n-1)) [1..]

elemSorted e l = e == last (takeWhile (<=e) l)

everyNth _ [] = []
everyNth n (x:xs) = x:(everyNth n $ drop (n-1) xs)
