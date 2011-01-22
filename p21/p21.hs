import Euler
import Data.List

main = print $ sum $ filter amicable [2..10000]

amicable a = let b = (sum $ init $ divisors a)
             in a /= b && (sum $ init $ divisors b) == a
