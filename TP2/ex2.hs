combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f xs ys = map(\(x,y)->f x y) (zip xs ys)