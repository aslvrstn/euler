import Char

main = print $ sum $ filter (\x -> x == (sum $ map (facts!!) $ digits x)) [3..8*(facts!!9)]

digits n = map digitToInt $ show n

facts = 1 : map (\n -> n*(facts!!(n-1))) [1..]
