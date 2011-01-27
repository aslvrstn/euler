import Data.List

main = do
         f <- readFile "keylog.txt"
         print $ ts $ concatMap pairs $ uniqLogins f

uniqLogins f = nub $ map init $ lines f

pairs [] = []
pairs (x:xs) = (map (\y->(x,y)) xs) ++ pairs xs

-- Stolen topological sort
univ u []  = u
univ u ((a,b):xs) = if a `elem` u then (univ1 b xs u) else (univ1 b xs (a:u))
  where
    univ1 x xs u = if x `elem` u then univ u xs else (univ (x:u) xs)
      
first []     xs = error "cycle"
first (u:us) xs = if all ((/=u).snd) xs then u else first us xs
           
remove x xs = [ (a,b) | (a,b)<-xs, a/=x && b/=x ]
              
ts :: [(Char, Char)] -> [Char]
ts xs = ts1 (univ [] xs) xs
  where
    ts1 :: [Char] -> [(Char,Char)] -> [Char]
    ts1 [] xs = []
    ts1 u xs = let a = first u xs in a : (ts1 (delete a u) (remove a xs))
