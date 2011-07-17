main = do
         f <- readFile "base_exp.txt"
         print $ pows f

pows :: String -> [(Integer, Integer)]
pows f = map (spanDrop ',') $ lines f
    where spanDrop c l = tailSnd $ span (c/=) l
          tailSnd (a,b) = (read a, read $ tail b)
