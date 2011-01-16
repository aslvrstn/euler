import Data.List

main = print (sort palindromes)

palindromes = (filter isPalindrome (concatMap (foo) [100..999]))

foo n = map (*n) [n..999]

isPalindrome n = let w = show n
                     fstT = fst (splitAt (div (length w + 1) 2) w)
                     sndT = snd (splitAt (div (length w) 2) w)
                     in fstT == reverse sndT
