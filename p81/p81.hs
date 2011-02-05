import Char

main = do
         f <- readFile "matrix.txt"
         print $ (minPath $ parseFile f)!!0!!0

minPath f = minPath'
            where 
                  rows = fromIntegral $ length f
                  cols = fromIntegral $ length (f!!0)
                  minPath' = map minPathRow [0..rows-2] ++ [scanr1 (+) $ last f]
                  minPathRow r = let ir = fromIntegral r
                                 in map (\c -> f!!ir!!c + min (minPath'!!(ir+1)!!c) (minPath'!!ir!!(c+1))) [0..cols-2] ++ [f!!ir!!(cols-1) + minPath'!!(ir+1)!!(cols-1)]

parseFile :: String -> [[Integer]]
parseFile f = map parseLine $ lines f
              where parseLine [] = []
                    parseLine l = read (takeWhile isDigit l) : (parseLine $ dropWhile (not.isDigit) $ dropWhile (isDigit) l)
