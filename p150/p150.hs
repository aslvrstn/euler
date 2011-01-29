main = print $ 0

rng = 0 : map (\n -> t!!n - 2^19) [1..500500]
      where
        t = 0 : map (\n -> (615949*t!!(n-1) + 797807) `mod` (2^20)) [1..500500]

realTri = realTri' 1 $ drop 1 rng
          where
            realTri' _ [] = []
            realTri' n t  = take n t : (realTri' (n+1) $ drop n t)

exampleTri = [[15],[-14,-7],[20,-13,-5],[-3,8,23,-26],[1,-4,-5,-18,5],[-16,31,2,9,28,3]]
