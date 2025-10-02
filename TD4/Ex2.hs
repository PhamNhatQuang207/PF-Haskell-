minListe :: (Ord a) => [a] -> Maybe a
minListe [] = Nothing
minListe (x:xs) = Just (foldr (\a b -> if a < b then a else b ) x xs)

minListe1 [] = Nothing
minListe1 (x:xs) = Just (foldl min x xs)