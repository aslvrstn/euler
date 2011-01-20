main = print $ maximum $ take 1000000 chainLen

chainLen = [0,1] ++ map (\n -> if (even n) then chainLen!!(n `div` 2)+1 else chainLen!!(3*n+1)+1) [2..]
