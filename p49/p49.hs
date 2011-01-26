import Euler
import Data.List

main = print $ nub $ filter arithSequence $ primeTriplePerms

primeTriplePerms = concatMap (subsq 3) $ map (\n -> filter (\x -> x >= 1000 && isPrime x) $ sort $ nub $ map read $ permutations $ show n) [1000..9999]

arithSequence (x1:x2:xs) = arithSequence' (x2-x1) x2 xs
                           where arithSequence' step last [] = True
                                 arithSequence' step last (y:ys) = if (y /= last+step) then False else arithSequence' step y ys

subsq 0 _ = [[]]
subsq _ [] = []
subsq n (x:xs) = (map (x:) $ subsq (n-1) xs)++subsq n xs
