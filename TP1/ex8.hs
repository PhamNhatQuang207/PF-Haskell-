somme :: [Int] -> Int
somme [] = 0
somme (x:l) = x + somme l

length1 :: [a] -> Int
length1 [] = 0
length1 l = somme (map (\_ -> 1) l)
