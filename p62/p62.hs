import Data.List
import Data.Function

main = print $ find (\c -> (length $ (filter isCube $ geThan (perms c) c)) == 5) cubes

cubes = map (flip (^) 3) [1..]

isCube x = (round (fromIntegral x ** (1/3))) ^ 3 == x

geThan l x = filter (x<=) l

perms x = nub $ filter (\p -> (length $ show p) == lx) $ map read $ permutations sx
    where sx = show x
          lx = length sx
