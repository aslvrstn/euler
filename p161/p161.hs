main = print $ t 2 9

t x 1 = if x `mod` 3 == 0 then 1 else 0
t 1 y = if y `mod` 3 == 0 then 1 else 0
t x y | x<=0 || y<=0 = 0
      | otherwise = ls x y + ls y x + is x y + is y x

ls a b = (t a (b-2)) * 2*(sum $ map (t 2) [0,3..a-3])
is a b = (t (a-3) 1)*(t a (b-1))
