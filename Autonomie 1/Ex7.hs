monMin :: Int -> Int -> Int
monMin a b = min a b 

monMinimum :: Int -> [Int] -> Int
monMinimum x [] = x
monMinimum x [a] = a
monMinimum x (a:b:l) = monMinimum x (monMin a b : l)