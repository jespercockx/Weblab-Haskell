insertSorted' :: Ord a => a -> [a] -> [a]
insertSorted' x [] = [x]
insertSorted' x (y:ys)
  | x <  y    = x:y:ys
  | x == y    = y:ys
  | otherwise = y:(insertSorted' x ys)

prop_expectFail = expectFailure (property prop_insertSorted1)
