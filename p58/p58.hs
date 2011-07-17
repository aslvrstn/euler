import Euler
import Data.List
import Data.List.Split
import Maybe

main = print $ ((fromJust $ findIndex (<0.1) $ map ratio [1..]) + 1) * 2 + 1

ratio n = (realToFrac $ diagPrimes!!n) / (realToFrac $ 1 + 4*n)

diagPrimes = 0 : map (\n -> (diagPrimes!!(n-1)) + (length $ filter isPrime $ layers!!n)) [1..]

layers = (take 1 corners) : chunk 4 (drop 1 corners)

corners = 1 : map (\n -> corners!!((fromInteger n)-1) + (((fromInteger n)-1) `div` 4 + 1)*2) [1..]
