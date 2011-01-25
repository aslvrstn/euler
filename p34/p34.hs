import Euler
import Data.Digits

main = print $ sum $ filter (\x -> x == (sum $ map (facts!!) $ digits 10 x)) [3..8*(facts!!9)]

facts = 1 : map (\n -> n*(facts!!(n-1))) [1..]
