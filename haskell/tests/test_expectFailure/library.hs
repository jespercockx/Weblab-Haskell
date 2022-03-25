elemSorted :: Ord a => a -> [a] -> Bool
elemSorted x [] = False
elemSorted x (y:ys)
  | x <  y    = False
  | x == y    = True
  | otherwise = elemSorted x ys

insertSorted :: Ord a => a -> [a] -> [a]
insertSorted x ys = [x]
