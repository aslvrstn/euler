import Data.List
import Data.List.Split
import Maybe

main = print $ maximumBy (\a b -> compare (snd a) (snd b)) $ zip [0..1000] (take 1000 repeatLen)

repeatLen = map (\x -> if fracTerminates x then 0 else foo x) [0..]
            where foo d = fromJust $ find (\n -> repeats n d) [1..]

fracTerminates d = all (==0) $ take d (drop d $ frac d)

repeats a d = repeats' a a $ frac d
              where repeats' n (-1) l     = False
                    repeats' n r l@(_:xs) = (all ((take n l)==) (take d (chunk n (drop n l)))) || repeats' n (r-1) xs

frac n = unfoldr (\x -> Just (div x n, 10 * (mod x n))) 10
