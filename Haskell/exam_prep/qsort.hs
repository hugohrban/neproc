qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x : xs) = qsort smaller ++ x : qsort bigger
  where
    smaller = filter (< x) xs
    bigger = filter (>= x) xs
