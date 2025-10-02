elementLast :: (a->Bool)-> [a] -> Maybe a
elementLast f = foldl (\acc x -> if f x then Just x else acc) Nothing