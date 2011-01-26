import Char
import Data.List.Split

main = do
         f <- readFile "words.txt"
         print $ length $ filter triWord $ wordify f

triWord w = let wv = wordVal w
            in elem wv (takeWhile (<=wv) tris)

wordVal w = sum $ map (\c -> (ord c) - (ord 'A')+1) w

wordify f = (split . dropBlanks . dropDelims . oneOf) "\"," f

tris = map (\n -> n * (n+1) `div` 2) [1..]
