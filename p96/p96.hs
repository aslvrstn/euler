import Data.List.Split
import Data.Char
import Data.List
import Data.Maybe

main = do
         f <- readFile "sudoku.txt"
         print $ solve $ head $ parse f

solve puzz = solve' $ listForm puzz

solve' puzz = let singled = keepRemovingUsed puzz
              in if solved singled then (Just singled) else (if solvable singled then listToMaybe $ mapMaybe solve' $ collapseOne singled else Nothing)

collapseOne (r:rs) = let collapsedRow = collapseOneR r
                     in if null collapsedRow then map (r:) (collapseOne rs) else map (flip (:) rs) collapsedRow
                     where collapseOneR [] = []
                           collapseOneR (p:ps) = if length p == 1 then map (p:) (collapseOneR ps) else map (\pi -> [pi]:ps) p

keepRemovingUsed puzz = let onePass = removeUsed puzz
                        in if onePass == puzz then puzz else keepRemovingUsed onePass

solvable puzz = all (all (\u -> (length u) /= 0)) puzz

solved puzz = all (all (\u -> (length u) == 1)) puzz

removeUsed puzz = transpose $ removeUsedRows $ transpose $ removeUsedRows puzz
    where 
        removeUsedRows puzz = map (\r -> map (remove (singlesRow r)) r) puzz
        singlesRow r = concat $ filter (\p -> (length p == 1)) r
        remove ss ps = if length ps == 1 then ps else ps \\ ss

--unused puzz = map (map (unused' puzz)) coords
--    where unused' puzz (x,y) = [1..9] \\ (puzz!!x ++ (transpose puzz)!!y ++ (sq puzz (x,y)))
--          sq puzz (x,y) = concatMap ((take 3) . (drop (3*(y `div` 3)))) $ take 3 $ drop (3*(x `div` 3)) puzz

listForm puzz = map (map (\x -> if (x == 0) then [1..9] else [x])) puzz

parse :: String -> [[[Int]]]
parse f = map (map (map digitToInt)) $ map (drop 1) $ chunk 10 $ lines f
