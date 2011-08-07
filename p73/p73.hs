import Data.List
import Data.Ratio

main = print $ length $ sol 12000

sol x = takeWhile (< (1 % 2)) . dropWhile (<= (1 % 3)) $ foldr1 mergeDD $ map (\n -> map (%n) [n `div` 3..n `div` 2]) [1..x]

mergeDD a [] = a
mergeDD [] b = b
mergeDD as@(a:as') bs@(b:bs') = case (compare a b) of
                                     LT -> a:(mergeDD as' bs)
				     EQ -> a:(mergeDD as' bs')
				     GT -> b:(mergeDD as bs')
