main = print $ ceiling $ 1/(product $ map (\t -> case t of (a,b,c) -> (fromIntegral a)/fromIntegral(c)) $ filter works [(a,b,c) | a<-[1..9], b<-[1..9], c<-[1..9]])

works (a,b,c) = let fa = fromIntegral a
                    fb = fromIntegral b
                    fc = fromIntegral c
                in (fa*10+fb)/(fb*10+fc) == fa/fc && not (a==c && b==c)
