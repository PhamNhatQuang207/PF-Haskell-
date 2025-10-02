filter1 :: (a->Bool) -> [a] -> [a]
filter1 f  = foldr (\x acc -> if f x then x : acc else acc) [] 