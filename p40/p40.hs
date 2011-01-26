import Char

main = print $ product $ map (digitToInt.(frac!!).(flip (-) 1).((^) 10)) [0..6]

frac = concatMap show [1..]
