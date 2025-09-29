f1 :: (Num a) => [a] -> a
f1 l = foldr (*) 1 l

f1' :: Num a => [a] -> a
f1' [] = 1
f1' (x:xs) = x * f1' xs

f2 :: (a -> Bool) -> [a] -> Maybe a
f2 p = foldr f Nothing
    where
        f e Nothing | p e =  Just e
                    | otherwise = Nothing
        f _ p = p

f2' p [] = Nothing
f2' p (x:xs) = 
    case f2' p xs of
        Just a -> Just a
        Nothing -> if p x then Just x else Nothing


f3 :: (Fractional a) => [a] -> a -> a -> a
f3 = foldr (\e f m s -> f (m+1) (s+e)) (/)

f3r :: Fractional t => [t] -> t -> t -> t
f3r [] m s = (/) m s
f3r (x:xs) m s = f3r xs (m+1) (s+x)  -- phải gọi helper từ cuối danh sách

f4 :: (Num a, Num b) => (a->b->b->b) -> [b] -> (a,b)
f4 f = foldr (\e (k,v) -> (k+1,f k e v)) (0,0)

f4' :: (Num a, Num b) => (a -> b -> b -> b) -> [b] -> (a,b)
f4' f xs = go xs
  where
    go []     = (0,0)  -- danh sách rỗng → 0 phần tử, tích lũy 0
    go (y:ys) =
      let (k, v) = go ys       -- đệ quy phần còn lại
      in (k+1, f k y v)        -- cập nhật k và áp dụng f
