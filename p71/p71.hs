import Data.List
import Data.Ratio

main = print $ head $ sol 1000000

sol x = dropWhile (>= (3 % 7)) . foldr1 mergeDD $ map (\n -> map (%n) $ reverse [1..(n*3) `div` 7+1]) [1..x]

mergeDD a [] = a
mergeDD [] b = b
mergeDD as@(a:as') bs@(b:bs') = case (compare a b) of
                                     GT -> a:(mergeDD as' bs)
				     EQ -> a:(mergeDD as' bs')
				     LT -> b:(mergeDD as bs')
