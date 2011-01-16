import Data.List

main = print $ product $ foldr (addMissingTo) [] $ map primeFactors [1..20]

addMissingTo l1 l2 = concat [l1 \\ l2, l2]

primeFactors 1 = [1]
primeFactors n = factorHelper n primes

factorHelper n (p:ps) | p*p > n         = [n]
                      | n `mod` p == 0  = p : factorHelper (n `div` p) (p:ps)
                      | otherwise       = factorHelper n ps

primes = 2 : (filter (\n -> nothingDivides n $ takeWhile (\m -> m*m <= n) primes) [3,5..])

nothingDivides n l = filter (\x -> mod n x == 0) l == []
