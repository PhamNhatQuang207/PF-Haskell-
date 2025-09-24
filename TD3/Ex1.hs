monMinimum :: Ord a => [a] -> Maybe a
monMinimum [] = Nothing
monMinimum l = Just (foldr1 min l)