main = print $ length $ concat $ takeWhile ((>0).length) $ map foo [1..]

foo n = filter ((==n).length.show) $ map (^n) [1..9]
