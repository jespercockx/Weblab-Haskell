
sorted :: Ord a => [a] -> Bool
sorted (x1:x2:xs) =
  x1 <= x2 && sorted (x2:xs)
sorted _          = True

prop_qsort_length :: [Int] -> Bool
prop_qsort_length xs = length (qsort xs) == length xs

prop_qsort_sorted :: [Int] -> Bool
prop_qsort_sorted xs = sorted (qsort xs)
