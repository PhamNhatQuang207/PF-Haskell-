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

voisins :: (Num a, Eq a) => [a] -> [a] -> [a]
voisins l1 l2 = filter(\e -> (e-1) `elem` l2 || (e+1) `elem` l2) l1

voisins' l1 l2 = [e| e <- l1,e `elem` ([e'+1 | e' <- l2] ++ [e'-1 | e' <- l2])]

moyenne :: (Fractional a) => [a] -> a
moyenne xs = s / t
    where
        (s, t) = foldr (\x (s,t) -> (s + x, t + 1)) (0, 0) xs