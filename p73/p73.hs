import Data.List
import Data.Ratio

print $ takeWhile (< (1 % 2)) . dropWhile (<= (1 % 3)) . List.nub . List.sort $ concatMap (\n -> map (%n) [1..n]) [1..12000]
