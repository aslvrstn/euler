main = print $ 0

poss :: [Integer]
poss = [read $ [a,b,c,d,e,f,g,h,i,j] | a <- ['1'], b <- ['0'..'3'], c <- ['0'..'9'], d <- ['0'..'9'], e <- ['0'..'9'], f <- ['0'..'9'], g <- ['0'..'9'], h <- ['0'..'9'], i <- ['3','7'], j <- ['0']]

check n = "1234567890" == (eother $ show n)

eother [] = []
eother [x] = [x]
eother (x1:x2:xs) = x1:(eother xs)
