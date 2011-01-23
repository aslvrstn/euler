import Data.List

main = print $ waysToMake 200 coins

coins = [1,2,5,10,20,50,100,200]

waysToMake n coins = let rscoins = reverse $ sort coins
                     in waysToMake' n rscoins
                     where waysToMake' 0 _      = 1
                           waysToMake' _ []     = 0
                           waysToMake' n (c:cs) = sum $ map (\x -> waysToMake' (n-x) cs) [0,c..n]
