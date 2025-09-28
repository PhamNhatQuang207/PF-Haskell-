m1 :: [Int] -> [Int]
m1 [] = [0]
m1 [a] = [a+1]
m1 (b:l) = (b+1): m1 l 

m2 :: [Int] -> [Int] -> [Int]
m2 [] [] = []
m2 (x:xs) [] = [x]
m2 [] (y:ys) = [y]
m2 (x:xs) (y:ys) = (x+y) : m2 xs ys

c1 :: [(Bool,Int)] -> [Int]
c1 l = case l of
        [] -> []
        ((True,k):l') -> (k+1):c1 l' 
        ((False,k):l') -> k: c1 l'

c2 :: [(Int -> Int -> Int,Int)]->[Int]->[Int]
c2 l1 l2 = case l1 of
           [] -> []
           ((f,a):l1') -> case l2 of
                         [] -> []
                         (b:l2') -> f a b : c2 l1' l2'