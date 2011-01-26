import Euler
import Data.List

main = print $ solve 4

solve n = find (\l -> all (\x -> (length $ nub $ primeFactors x) == n) l) $ consecs n [1..]

consecs n l@(x:xs) = let taken = take n l
                     in if length taken < n then [] else taken:(consecs n xs)
