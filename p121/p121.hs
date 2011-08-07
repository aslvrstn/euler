main = print $ ceiling $ 1/(p 8 15)-1

p 0 _ = 1
p x y | x > y = 0
p x y | otherwise = (1 / (y+1)) * (p (x-1) (y-1)) + (y / (y+1)) * (p x (y-1))
