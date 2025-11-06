quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort lower ++ [x] ++ quickSort upper
    where
        lower = [z|z <- xs, z <= x]
        upper = [z|z <- xs, z > x]

groupe :: (Eq a) => [a] -> [[a]]
groupe [] = []
groupe [x] = [[x]]
groupe (x:y:xs) 
    | x == y = let (g:gs) = groupe (y:xs) in (x:g):gs
    | otherwise = [x] : groupe (y:xs)

