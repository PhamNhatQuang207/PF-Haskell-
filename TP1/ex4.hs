somme :: [Int] -> Int
somme [] = 0
somme (x:l) = x + somme l
