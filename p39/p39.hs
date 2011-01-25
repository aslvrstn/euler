import Data.List

main = print $ maximumBy (\a b -> compare (snd a) (snd b)) $ zip [2,4..1000] $ map (length.sols) [2,4..1000]

sols p = filter tri [(a,b,c) | a <- [1..div p 2], b <- [a..div (p-a) 2], let c = p-a-b]

tri (a,b,c) = a^2+b^2==c^2
