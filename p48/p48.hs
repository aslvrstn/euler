main = print $ drop (length ssum - 10) ssum

ssum = show $ sum $ map (\n -> n^n) [1..1000]
