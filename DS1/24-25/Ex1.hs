import GHC.Read (list, paren)
--Ex1.1

estElement :: Eq a => a -> [a] -> Bool
estElement _ [] = False
estElement v (x:xs) 
    | v == x = True
    | otherwise = estElement v xs

estElement' :: Eq a => a -> [a] -> Bool
estElement' x = foldr (\a acc -> (x == a) || acc) False

--Ex1.2
estPrefixe :: Eq a => [a] -> [a] -> Bool
estPrefixe [] _ = True
estPrefixe _ [] = False
estPrefixe (x:xs) (y:ys) 
    | x == y = estPrefixe xs ys
    | otherwise = False

--Ex1.3
repliquer :: Int -> a -> [a]
repliquer 0 _ = []
repliquer n x = x : repliquer (n-1) x

--Ex1.4 
decouper :: Int -> [a] -> Maybe ([a],[a])
decouper 0 l = Just ([],l)
decouper n (x:xs) = case decouper (n-1) xs of
                        Just (l1,l2) -> Just (x:l1,l2)
                        Nothing -> Nothing
decouper _ _ = Nothing

--Ex1.5
estDIndex :: Eq a => a -> [a] -> Maybe Int
estDIndex a  = go a 0 
            where
                go _ _ [] = Nothing
                go a n (x:xs) = if a == x then Just n else go a (n+1) xs

estDIndex' :: Eq a => a -> [a] -> Maybe Int
estDIndex' x = foldr (\a acc -> if x == a then Just 0 else (+1) <$> acc ) Nothing 

--Ex1.6
moyennePonderee :: Fractional a => [a] -> [a] -> a
moyennePonderee l1 l2 = s/p
    where
        (s,p) = foldr (\(som,pds) (v,w) -> (som + v,pds + v*w)) (0,0) (zip l1 l2)

moyennePonderee' l1 l2 = s/p
    where
        s = foldr (+) 0 l1
        p = foldr (+) 0 (zipWith (*) l1 l2)