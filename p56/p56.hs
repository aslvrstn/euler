import Data.Digits

main = print $ maximum $ map (sum.digits 10) [a^b | a <- [1..99], b <- [1..99]]
