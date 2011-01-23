import Char

main = print $ sum $ answer 5

answer pow = filter (flip canDigitPow pow) $ takeWhile (\x -> x <= (length $ show x) * 9^pow) [2..]

canDigitPow n pow = sum (map ((^pow).digitToInt) $ show n) == n
