f1 :: [Bool] -> Bool
f1 l = foldl (||) False l

f1' :: [Bool] -> Bool
f1' [] = False
f1' (x:xs) = x || f1' xs

f2 :: (a->Bool) -> [a] -> [a]
f2 p l = foldl (\l' e -> if p e then l' else e:l') [] l

f2' :: (a -> Bool) -> [a] -> [a]
f2' p l = go l []
        where
            go [] acc = acc
            go (x:xs) acc
                | p x = go xs acc
                | otherwise = go xs (x:acc)

f3 :: (Num a) => [a] -> a
f3 = snd . foldl(\(i,v) x -> (i+1,v+x^i)) (1,0)

f3' :: Num a => [a] -> a
f3' l = go l 1
    where
        go [] _ = 0
        go (x:xs) n = x^n + go xs (n+1)

f4 :: (Num a) => (a -> Bool) -> [a] -> Maybe a
f4 p l = foldl f Nothing l
  where
    f (Just a) e | p e       = Just (a * e)
                 | otherwise = Just a
    f Nothing e   | p e       = Just e
                  | otherwise = Nothing

f4' :: (Num a) => (a -> Bool) -> [a] -> Maybe a
f4' p [] = Nothing
f4' p (x:xs)
  | p x       = Just (go x xs)   -- bắt đầu tính tích từ x
  | otherwise = f4 p xs          -- bỏ qua, tìm tiếp
  where
    go acc [] = acc
    go acc (y:ys)
      | p y       = go (acc * y) ys
      | otherwise = go acc ys

