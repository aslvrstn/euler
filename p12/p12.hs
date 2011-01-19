import Euler
import Data.List

--main = print $ head $ filter ((>500).length.divisors) $ tris
main = print $ head $ filter ((>500).numDivisors) $ tris

tris = 0 : (map (\n -> tris!!(fromIntegral(n)-1)+n) [1..])

--divisors n = nub $ map product $ subsequences $ primeFactors n
numDivisors n = product $ map ((+1).length) $ group $ primeFactors n
