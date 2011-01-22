import Char
import Data.List

main = print $ (sort $ map (map Char.intToDigit) (permutations [0..9]))!!999999
