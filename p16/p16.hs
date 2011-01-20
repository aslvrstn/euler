import Data.Bits
import Char

main = print $ sum $ map digitToInt $ show $ shiftL (1::Integer) 1000
