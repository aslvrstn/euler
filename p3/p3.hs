import Euler

main = print $ filter (\n -> mod 600851475143 n == 0) $ takeWhile (<775146) primes
