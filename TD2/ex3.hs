insertBeforeWhen :: (a -> Bool) -> a -> [a] -> [a]
insertBeforeWhen p y [] = [y]
insertBeforeWhen p y (x:xs) | p x       = y : x : xs
                            | otherwise = x : insertBeforeWhen p y xs

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl (\sorted x -> insertBeforeWhen (>x) x sorted) []
