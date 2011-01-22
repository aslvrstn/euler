main = print $ length $ filter (==0) $ take (12*99) $ drop 12 dow1st

dow1st = 1 : (map (\n -> mod (dow1st!!(n-1) + (daysInMonth (div n 12))!!(mod n 12)) 7) [1..])

daysInMonth yr = [31, if (yr `mod` 4 == 0) && (yr `mod` 400 /= 0) then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
