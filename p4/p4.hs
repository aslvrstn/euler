main = print (filter isPalindrome (concatMap (foo) [100..999]))

foo n = map (*n) [n..999]

isPalindrome n = let w = show n
                     tup = splitAt (div (length w) 2) w
                     in fst tup == reverse (snd tup)
