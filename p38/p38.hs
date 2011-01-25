import Data.List
import Maybe

main = print $ head $ reverse $ sort $ concatMap (\x -> mapMaybe (flip pandigitalAgainst [1..x]) [1..10^( (10 `div` x - 1))]) [2..9]

pandigitalAgainst k l = let foo = concatMap (show.(*k)) l
                        in if isPandigital foo then Just(foo) else Nothing

isPandigital n = (sort n) == "123456789"
