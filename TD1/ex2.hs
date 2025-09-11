-- 2.1
compterOcc :: Int -> [Int] -> Int
compterOcc a [] = 0
compterOcc a (x:xs) | a == x = 1 + compterOcc a xs
                    | otherwise = compterOcc a xs

eliminer :: Int -> [Int] -> [Int]
eliminer a [] = []
eliminer a (x:xs) | a == x = eliminer a xs
                  | otherwise = x : eliminer a xs

compter1 :: [Int] -> [(Int,Int)]
compter1 [] = []
compter1 (a:l) = (a, compterOcc a (a:l)) : compter1 (eliminer a (a:l))


--2.2
comptElim  :: Int -> [Int] -> (Int,[Int])
comptElim a [] = (0,[])
comptElim a (x:xs) 
  | a == x = (1+n,el)
  | otherwise = (n,x : el)
  where 
  (n, el) = comptElim a xs

compter2 :: [Int] -> [(Int,Int)]
compter2 [] = []
compter2 (a:l) = (a,n) : compter2 el
  where
  (n,el) = comptElim a (a:l) 

-- Complexite O(N^2)

main = do
  print (compterOcc 1 [1,2,1,3,1])
  print (eliminer 1 [1,2,1,3,1])
  print (compter1 [1,2,1,3,1])
  print (comptElim 1 [1,2,1,3,1])
  print (compter2 [1,2,1,3,1])