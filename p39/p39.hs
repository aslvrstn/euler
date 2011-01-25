main = print $ map (length.sols) [1..1000]

sols p = filter tri [(a,b,c) | a <- [1..p-2], b <- [a..p-a-1], let c = p-a-b]

tri (a,b,c) = a^2+b^2==c^2
