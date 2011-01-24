module Euler (digits, fibs, primes, isPrime, primeFactors, divisors) where

import Data.List
import Char

digits n = map digitToInt $ show n

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes = 2 : (filter (\n -> nothingDivides n $ takeWhile (\m -> m*m <= n) primes) [3,5..])

nothingDivides n l = filter (\x -> mod n x == 0) l == []

isPrime n = (n > 1) && (length $ primeFactors n) == 1

primeFactors 1 = [1]
primeFactors n = factorHelper n primes

factorHelper n (p:ps) | p*p > n         = [n]
                      | n `mod` p == 0  = p : factorHelper (n `div` p) (p:ps)
                      | otherwise       = factorHelper n ps

divisors n = nub $ map product $ subsequences $ primeFactors n
