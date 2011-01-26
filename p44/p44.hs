import Data.List

main = print $ find (\p -> elemSorted (fst p - snd p) pents && elemSorted (fst p + snd p) pents) increasingDPairs

increasingDPairs = concatMap (\x -> pairsWithLast $ take x pents) [2..]

pairsWithLast [] = []
pairsWithLast l = let r = reverse l
                  in map (\x -> (head r, x)) (tail r)

allPairs [] = []
allPairs (x:xs) = map (\y -> (x,y)) xs ++ allPairs xs

elemSorted e l = e `elem` (takeWhile (<=e) l)

pents = map (\n -> n*(3*n-1) `div` 2) [1..]
