main = do
         f <- readFile "base_exp.txt"
         print $ solve $ pows f

solve pows = let maxLens = map maxLen $ pows
             in filter ((len $ head pows)<=) maxLens

len (b,e) = length $ show $ b^e

maxLen (b,e) = let l = logBase 10 (fromIntegral b)
               in (ceiling l)*e+1

pows f = map (spanDrop ',') $ lines f
    where spanDrop c l = tailSnd $ span (c/=) l
          tailSnd (a,b) = (read a, read $ tail b)
