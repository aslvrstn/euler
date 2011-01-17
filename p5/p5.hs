import Data.List
import Euler

main = print $ product $ foldr (addMissingTo) [] $ map primeFactors [1..20]

addMissingTo l1 l2 = concat [l1 \\ l2, l2]
