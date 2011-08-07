main = print $ 0

winning [] = false
winning (0:xs) = winning xs
--winning xs@(x:xs') = all (==x) xs || realwin xs
winning xs = realwin xs
    where realwin xs = any (not . winning) (moves xs)
