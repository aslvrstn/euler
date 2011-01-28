import Euler
import Data.List

main = print $ find (<0.1) $ map ratio [1..]

diagPrimes = map (\n -> length $ filter isPrime $ layer n) [0..]

totDiag = map (length.layer) [0..]

ratio n = (realToFrac $ diagPrimes!!n) / (realToFrac $ totDiag!!n)

layer n = take (1 + 4*n) corners

corners = 1 : map (\n -> corners!!((fromInteger n)-1) + (((fromInteger n)-1) `div` 4 + 1)*2) [1..]
