import Data.Ratio

main = print $ length $ filter numLonger $ map iter [0..999]

numLonger f = (length.show.numerator) f > (length.show.denominator) f

iter n = 1 + (1/fracPart!!n)

fracPart = (2%1) : map (\n -> 2 + (1 / fracPart!!(n-1))) [1..]
