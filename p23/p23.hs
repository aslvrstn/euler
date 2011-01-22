import Euler
import Data.List

main = print $ sum $ filter (not.(twoCanSum (takeWhile (<=28123) abundants))) [1..28123]

twoCanSum [] _ = False
twoCanSum l@(x:xs) n = ((n-x > 0) && (n-x) `elem` l) || twoCanSum xs n

abundants = filter (\n -> (sum $ init $ divisors n) > n) [1..]
