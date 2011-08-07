main = print $ 0

cuboids = 0 : map (\z -> cuboids!!(z-1) + length [(x,y,z) | y <- [1..z], x <- [1..y], isSquare $ (x+y)^2+z^2]) [1..]

isSquare n = sq * sq == n
  where sq = floor $ sqrt $ (fromIntegral n::Double)
