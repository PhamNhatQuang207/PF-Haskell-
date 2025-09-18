alterne :: [Int] -> [Int]
alterne [] = []
alterne [x] = [x]
alterne (x:_:xs) = x : alterne xs