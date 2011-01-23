import Data.List
import Maybe

main = print $ sum $ nub $ concatMap pandig $ permutations "123456789"

pandig l = mapMaybe (\p -> pandig' p) [(take x l, take y (drop x l), drop (x+y) l) | x <- [1..7], y <- [1..8-x]]
           where pandig' (x,y,z) = let
                                     xr = read x
                                     yr = read y
                                     zr = read z
                                     in if xr*yr==zr then Just(zr) else Nothing
