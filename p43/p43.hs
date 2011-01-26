import Euler
import Data.List

main = print $ sum $ map read $ filter hasProp $ permutations "0123456789"

hasProp n = all id $ map (\p -> (read $ map (n!!) (fst p)) `mod` (snd p) == 0) pairs

pairs = zip (map (\x -> [x,x+1,x+2]) [1..7]) primes
