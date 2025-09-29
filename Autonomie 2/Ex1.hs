f1 l = map (\x -> x/2) $ filter (\y -> y >= 4) l

f1' = map (/2).filter(>=4) 

f2 l = concatMap (\x -> x:l) (filter (\p -> p<2) l)

f2' l = concatMap (:l) (filter (<2) l)

flip1 :: (a->b->c)->b->a->c
flip1 f x y = f y x

f3 l1 l2 = map (\x -> x/2) $ zipWith (\x y -> x^y) l1 l2

f3' l1 l2 = map (/2) $ zipWith (^) l1 l2

f4 r = foldl (\p m -> p + m) 0 $ foldr (\x acc -> acc / x) 1 r

f4' r = foldl (+) 0 $ foldr (flip (/)) 1 r

