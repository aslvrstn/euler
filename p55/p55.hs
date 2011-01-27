import Data.List
import Maybe

main = print $ length $ filter isLychrel [1..9999]

isLychrel n = isNothing $ find isPalindrome $ take 50 $ drop 1 $ iterate revAndAdd n

revAndAdd n = n + (read.reverse.show $ n)

isPalindrome n = let sn = show n in sn == reverse sn
