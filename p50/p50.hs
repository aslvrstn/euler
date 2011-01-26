import Euler
import Data.List

main = print $ maximumBy (\a b -> compare (fst a) (fst b)) $ primeSumsUnder 1000000

primeSumsUnder n = concatMap (\x -> takeWhile ((<n).snd) $ primeSumsStartingAt x) [0..(length (takeWhile (<n) primes))]

primeSumsStartingAt n = filter (isPrime.snd) $ zip [1..] $ map (sum.(flip take (drop n primes))) [1..]
