(!!!) :: [a] -> Int -> a
(!!!) l n 
  | null l = error "empty list"
  | n < 0 = error "negative index"
  | n == 0 = head l
  | n > length l = error "index too large"
  | otherwise = head (drop n l)

addL :: [a] -> [a] -> [a]
addL [] l2 = l2
addL l1 [] = l1
addL (x:l) l2 = x : addL l l2

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:l) = addL x (flatten l) 

map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x:l) = (f x) : map1 f l