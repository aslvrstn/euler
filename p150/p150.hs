main = print $ 0

rng = 0 : map (\n -> t!!n - 2^19) [1..500500]

t = 0 : map (\n -> (615949*t!!(n-1) + 797807) `mod` (2^20)) [1..500500]
