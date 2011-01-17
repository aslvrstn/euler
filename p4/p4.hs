import Data.List

main = print $ sort palindromes

palindromes = filter isPalindrome $ concatMap foo [100..999]

foo n = map (*n) [n..999]

isPalindrome n = let w = show n
                 in w == reverse w
