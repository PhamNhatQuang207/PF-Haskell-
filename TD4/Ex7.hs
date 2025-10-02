prefixes :: [a] -> [[a]]
prefixes = foldl (\acc x -> let s = last acc ++ [x] in acc ++ [s] ) [[]]