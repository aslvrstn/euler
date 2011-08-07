main = print $ foo

foo = let s2 = sqrt(2)-1 in [(b,r) | b <- [1..], let s = s2*(fromIntegral b), r <- [floor s..ceiling s], (b+r)*(b-r)-2*b*r == b-r]
