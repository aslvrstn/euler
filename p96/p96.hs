import Data.List.Split
import Data.Char
import Data.List

main = do
         f <- readFile "sudoku.txt"
         print $ parse f

--solve puzz = let singled = keepFillingSingles puzz
--             in if solved singled then (Just singled) else (if solvable singled then solve puzz else Nothing)

--keepFillingSingles puzz = let onePass = fillSingles puzz
--                          in if (onePass == puzz) then puzz else keepFillingSingles onePass

solvable puzz = all (all (\u -> (length u) /= 0)) puzz

solved puzz = all (all (\u -> (length u) == 1)) puzz


removeUsed puzz = transpose $ removeUsedRows $ transpose $ removeUsedRows puzz
    where 
        removeUsedRows puzz = map (\r -> remove (singles r) r) puzz
        singles r = concat $ filter (\p -> (length p == 1)) r
        remove s ps = map (\p -> if length p == 1 then p else p \\ s) ps

--fillSingles puzz = map (map (\c@(x,y) -> let v = puzz!!x!!y in if v == 0 then (let uu = unused puzz c in if length uu == 1 then head uu else 0) else v)) coords

--coords = map (\x -> map (\y -> (x,y)) [0..8]) [0..8]

--unused puzz = map (map (unused' puzz)) coords
--    where unused' puzz (x,y) = [1..9] \\ (puzz!!x ++ (transpose puzz)!!y ++ (sq puzz (x,y)))
--          sq puzz (x,y) = concatMap ((take 3) . (drop (3*(y `div` 3)))) $ take 3 $ drop (3*(x `div` 3)) puzz

listForm puzz = map (map (\x -> if (x == 0) then [1..9] else [x])) puzz

parse :: String -> [[[Int]]]
parse f = map (map (map digitToInt)) $ map (drop 1) $ chunk 10 $ lines f
