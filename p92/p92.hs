import Data.Digits

main = print $ 0

--length $ filter id $ map goesTo89 [1..9999999]
--length $ filter id $ take 10000000 (tail endsAt89)

endsAt89 = False : False : map (\x -> if x==89 then True else endsAt89!!(sum $ map (^2) $ digits 10 x)) [2..]

goesTo89 89 = True
goesTo89 n = goesTo89 $ sum $ map (^2) $ digits 10 n
