main = print (length (concatMap (foo) [100..999]))

foo n = map (*n) [n..999]
