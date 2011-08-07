import Data.List
import Data.Ratio

maint = print $ sol

sol = takeWhile (< (1 % 2)) . dropWhile (<= (1 % 3)) . nub . sort $ concatMap (\n -> map (%n) [n `div` 3..n `div` 2]) [1..8]
