main = print $ solve [1,3,3] 100

solve p n = winning!!(index p n)
    where winning = repeat 0

index [] n = 0
index (p:ps) n = p + 10^(length $ show n)*(index ps n)

moves [] = []
moves (0:xs) = moves xs
moves (x:xs) = map (x:) $ moves xs ++ concatMap (\s -> map ((-) s) xs) [1..x]

--winning [] = false
--winning (0:xs) = winning xs
--winning xs@(x:xs') = all (==x) xs || realwin xs
--winning xs = realwin xs
--   where realwin xs = any (not . winning) (moves xs)
