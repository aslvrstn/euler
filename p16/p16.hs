import Data.Bits
import Data.Digits

main = print $ sum $ digits 10 $ shiftL (1::Integer) 1000
