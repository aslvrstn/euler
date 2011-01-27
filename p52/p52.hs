import Data.List

main = print $ find (\x -> all ((arePerms $ show x).show) $ take 6 $ mults x) [2..]

mults x = map (*x) [1..]

arePerms a b = sort a == sort b
