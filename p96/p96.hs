import Data.List.Split
import Data.Char
import Data.List
import Data.Maybe

main = do
         f <- readFile "sudoku.txt"
         print $ euler $ parse f

euler puzzles = sum $ map (\p -> read $ map intToDigit (take 3 $ head $ solve p)) puzzles

solve puzz = singleForm $ fromJust $ solve' $ listForm puzz

solve' puzz = let singled = keepRemovingUsed puzz
              in if solved singled then (Just singled) else (if solvable singled then listToMaybe $ mapMaybe solve' $ collapseOne singled else Nothing)

collapseOne [] = []
collapseOne (r:rs) = let collapsedRow = collapseOneR r
                     in if null collapsedRow then map (r:) (collapseOne rs) else map (flip (:) rs) collapsedRow
                     where collapseOneR [] = []
                           collapseOneR (p:ps) = if length p == 1 then map (p:) (collapseOneR ps) else map (\pi -> [pi]:ps) p

keepRemovingUsed puzz = let onePass = removeUsed puzz
                        in if onePass == puzz then puzz else keepRemovingUsed onePass

solvable puzz = all (all (\u -> (length u) /= 0)) puzz

solved puzz = all (all (\u -> (length u) == 1)) puzz && all (all usedAlright) (used $ singleForm puzz)
    where usedAlright u = all (\g -> (length g == 3)) $ group $ sort u

removeUsed puzz = zipWith (zipWith (\uu u -> if length uu == 1 then uu else (uu \\ u))) puzz (nub $ used $ singleForm puzz)

used puzz = map (map (used' puzz)) coords
    where used' puzz (x,y) = (puzz!!x ++ (transpose puzz)!!y ++ (sq puzz (x,y)))
          sq puzz (x,y) = concatMap ((take 3) . (drop (3*(y `div` 3)))) $ take 3 $ drop (3*(x `div` 3)) puzz

coords = map (\x -> map (\y -> (x,y)) [0..8]) [0..8]

listForm puzz = map (map (\x -> if (x == 0) then [1..9] else [x])) puzz

singleForm lpuzz = map (map (\u -> if (length u == 1) then head u else 0)) lpuzz

parse :: String -> [[[Int]]]
parse f = map (map (map digitToInt)) $ map (drop 1) $ chunk 10 $ lines f
