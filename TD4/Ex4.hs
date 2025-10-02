element :: Eq a => a -> [a] -> Bool
element x  = foldr (\a acc -> (a == x) || acc) False 