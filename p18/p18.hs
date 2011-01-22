main = do
         f <- readFile "triangle.txt"
         print $ maxPath $ triangle f

maxPath [] = 0
maxPath t@([root]:_) = root + max (maxPath $ lTriangle t) (maxPath $ rTriangle t)

lTriangle ([root]:rows) = map init rows

rTriangle ([root]:rows) = map (drop 1) rows

triangle :: String -> [[Integer]]
triangle f = map (\x -> map read x) (map words $ lines f)
