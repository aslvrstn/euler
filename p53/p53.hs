import Euler

main = print $ length $ filter (>1000000) $ concatMap (\n -> map (choose n) [1..n]) [1..100]

choose n r = facts!!n `div` ((facts!!r) * (facts!!(n-r)))

facts = 1 : map (\n -> n*(facts!!((fromInteger n)-1))) [1..]
