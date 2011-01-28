import Data.Char
import Data.Bits
import Data.List
import Maybe

main = do
         f <- readFile "cipher1.txt"
         print $ sum $ map (ord) $ sol $ numParse f
         
sol f = fromJust $ find (isInfixOf " the ") $ allPossibilities f

allPossibilities l = map (\k -> toString $ xorWith (key k) $ l) [[x1,x2,x3] | x1 <- ['a'..'z'], x2 <- ['a'..'z'], x3 <- ['a'..'z']]

toString e = map chr e
key l = map ord l

xorWith key [] = []
xorWith key l = (zipWith xor key l) ++ (xorWith key (drop (length key) l))

numParse :: String -> [Int]
numParse f = map read $ parse f

parse [] = []
parse f = (takeWhile isAlphaNum f) : (parse $ dropWhile (not.isAlphaNum) $ dropWhile isAlphaNum f)
