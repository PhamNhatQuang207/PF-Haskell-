voisins :: (Num a, Eq a) => [a] -> [a] -> [a]
voisins l1 l2 = filter(\e -> (e-1) `elem` l2 || (e+1) `elem` l2) l1

voisins' l1 l2 = [e| e <- l1,e `elem` ([e'+1 | e' <- l2] ++ [e'-1 | e' <- l2])]


divCommun :: Integral a => [a] -> [a] -> Bool
divCommun [] _ = False
divCommun _ [] = True
divCommun (x:xs) (y:ys) 
    | mod y x == 0 = divCommun (x:xs) ys
    | otherwise = divCommun xs (y:ys)

divCommun' l1 l2 = any(\e -> all(\e' -> e' `mod` e == 0) l2) l1

moyenne :: (Fractional a) => [a] -> a
moyenne xs = s / t
    where
        (s, t) = foldr (\x (s,t) -> (s + x, t + 1)) (0, 0) xs

estPrefixe :: (Eq a) => [a] -> [a] -> Bool
estPrefixe [] _ = True
estPrefixe _ []      = False
estPrefixe (x:xs) (y:ys) 
    | x == y = estPrefixe xs ys
    | otherwise = False

compteOcc :: Eq a => [a] -> [a] -> Int
compteOcc l [] = 0
compteOcc l (x:xs) 
    | estPrefixe l (x:xs) = 1 + occ
    | otherwise = occ
    where
        occ = compteOcc l xs