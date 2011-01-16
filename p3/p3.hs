main = print $ filter (\n -> mod 600851475143 n == 0) $ takeWhile (<775146) primes

primes = 2 : (filter (\n -> nothingDivides n $ takeWhile (\m -> m*m <= n) primes) [3,5..])

nothingDivides n l = filter (\x -> mod n x == 0) l == []
