main = print $ choose 40 20

choose n k = product [k+1..n] `div` product [1..k]
