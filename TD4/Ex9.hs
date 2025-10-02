element1 :: (a->Bool)-> [a] -> Maybe a
element1 f = foldr (\x acc -> if f x then Just x else acc ) Nothing