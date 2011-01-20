import Data.List

main = print $ maximumBy (\a b -> compare (snd a) (snd b)) $ map (\n -> (n, length $ chain n)) [1..1000000]

chain 1 = [1]
chain n = if (even n) then n : chain (n `div` 2) else n : chain (3*n+1)

--chainLen = [0,1] ++ map (\n -> if (even n) then chainLen!!(n `div` 2)+1 else chainLen!!(3*n+1)+1) [2..]
