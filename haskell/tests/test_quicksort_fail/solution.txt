
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) =
  [x] ++ qsort smaller ++ qsort larger
    where
      smaller = [ y | y <- xs , y < x ]
      larger  = [ y | y <- xs , y > x ]
