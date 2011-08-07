main = print $ 0

filledIn :: [Integer]
filledIn = map (read . intersp "1234567890") holes

holes = [[a,b,c,d,e,f,g,h,i] | a <- ['0'..'9'], b <- ['0'..'9'], c <- ['0'..'9'], d <- ['0'..'9'], e <- ['0'..'9'], f <- ['0'..'9'], g <- ['0'..'9'], h <- ['3','7'], i <- ['0']]

intersp xs [] = xs
intersp [] ys = ys
intersp (x:xs) y = x:(intersp y xs)
