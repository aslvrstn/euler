main = print $ (sum $ map wordLen [1..999]) + (length "oneThousand")

wordLen n = let
            swl = smallWordLen $ mod n 100
            in
            (if (n>99) then throughTeensLen!!(div n 100) + (length "hundred") + (if (swl > 0) then (length "and") else 0) else 0) + (smallWordLen $ mod n 100)

smallWordLen n = if (n<20) then throughTeensLen!!n else tensLen!!(div (mod n 100) 10) + throughTeensLen!!(mod n 10)

tensLen = map length tens
throughTeensLen = map length throughTeens

tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"];

throughTeens = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
