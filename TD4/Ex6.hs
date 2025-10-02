suffixes :: [a] -> [[a]]
suffixes = foldr (\x acc -> let s = x : head acc in s : acc ) [[]]