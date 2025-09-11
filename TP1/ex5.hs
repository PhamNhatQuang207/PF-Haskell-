last1 :: [Int] -> Int
last1 [] = error "last1: empty list"
last1 [x] = x
last1 (_:l) = last1 l

init1 :: [Int] -> [Int]
init1 [] = error "init1: empty list"
init1 [x] = []
init1 l = take ((length l) -1) l