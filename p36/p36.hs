import Data.Bits
import Data.Digits
import Data.List

main = print $ sum $ filter (\n -> (isPalindrome $ digits 10 n) && (isPalindrome $ toBinary n)) [1..999999]

toBinary :: Int -> String
toBinary n = dropWhile (=='0') $ map (\b -> if testBit n ((bitSize n)-b) then '1' else '0') [0..(bitSize n)]

isPalindrome l = l == (reverse l)
