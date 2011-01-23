main = print $ answer 1001

answer n = sum $ take (n*2-1) corners

corners = 1 : map (\n -> corners!!(n-1) + ((n-1) `div` 4 + 1)*2) [1..]
