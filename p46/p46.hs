import Euler
import Data.List

main = print $ find (not.snd) (zip oddComps (map canMake oddComps))

canMake n = any id $ map (\p -> (last $ takeWhile (<=(n-p)) (map (*2) squares))+p==n) $ takeWhile (<n) primes

oddComps = filter (not.isPrime) [9,11..]

squares = map (^2) [1..]
