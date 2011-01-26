import Euler
import Data.List
import Data.Digits

main = print $ maximum $ concatMap (\n -> map (unDigits 10) $ filter (isPrime.(unDigits 10)) $ permutations [1..n]) [2..7]
