import Euler

main = print $ filter ((==0).(mod 600851475143)) $ takeWhile (<775146) primes
