main = do
         f <- readFile "triangle.txt"
         print $ maxPath $ triangle f

maxPath tri = let maxPath' = map maxPaths $ zip [0..] (tri++[repeat 0])
                  maxPaths (r,l) 
                      | r >= (length tri) = repeat 0
                      | otherwise         = map (\c -> tri!!r!!c + max (maxPath'!!(r+1)!!c) (maxPath'!!(r+1)!!(c+1))) [0..(length l)-1]
              in maxPath'!!0!!0

triangle f = map (\x -> map read x) (map words $ lines f)
