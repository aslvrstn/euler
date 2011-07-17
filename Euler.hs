module Euler where

import Data.List

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes = 2 : (filter (\n -> nothingDivides n $ takeWhile (\m -> m*m <= n) primes) [3,5..])
    where nothingDivides n l = null $ filter (\x -> mod n x == 0) l

isPrime n = (n > 1) && (length $ primeFactors n) == 1

primeFactors 1 = [1]
primeFactors n = helper n primes
    where helper n (p:ps) | p*p > n         = [n]
                          | n `mod` p == 0  = p : helper (n `div` p) (p:ps)
                          | otherwise       = helper n ps

divisors n = nub $ map product $ subsequences $ primeFactors n
