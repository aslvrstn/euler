import Euler
import Data.List

main = print $ maximumBy (\a b -> compare (snd a) (snd b)) $ map (\x -> (x, generatedPrimes (fst x) (snd x))) [(a,b) | a <- [-999..999], b <- takeWhile (<1000) primes]

generatedPrimes a b = length $ takeWhile isPrime $ map (quad a b) [0..]

quad a b = \x -> x^2 + a*x + b
