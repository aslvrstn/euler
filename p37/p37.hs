import Euler
import Data.Digits

main = print $ sum $ take 11 $ filter (\n -> lTrunctable isPrime n && rTrunctable isPrime n) [10..]

lTrunctable f n = let dig = digits 10 n
                  in all f $ map (\n -> unDigits 10 $ drop n dig) [0..(length dig)-1]

rTrunctable f n = let dig = digits 10 n
                  in all f $ map (\n -> unDigits 10 $ take n dig) [1..length dig]

