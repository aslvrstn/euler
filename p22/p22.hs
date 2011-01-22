import Char
import Data.List
import Data.List.Split

main = do
         f <- readFile "names.txt"
         print $ sum $ wordScores $ sort $ names f

wordScores names = zipWith (\w i -> (value w) * i) names [1..]

value w = sum $ map (\c -> ord c - (ord 'A' - 1)) w

names f = (split . dropBlanks . dropDelims . oneOf) "\",\n" f
