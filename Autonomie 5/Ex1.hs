f1 :: Num a => Maybe a -> Maybe a -> Maybe a -> Maybe a
f1 m1 m2 m3 = case m1 of
  Just v1 -> case m2 of
    Just v2 -> case m3 of
      Just v3 -> Just $ v1 * v2 + 2 * v3
      Nothing -> Nothing
    Nothing -> Nothing
  Nothing -> Nothing

f1' :: Num a => Maybe a -> Maybe a -> Maybe a -> Maybe a
f1' m1 m2 m3 = (\v1 v2 v3 -> v1 * v2 + 2 * v3) <$> m1 <*> m2 <*> m3

f2 :: (Eq a, Num b) => [(a,b)] -> [a] -> Maybe b
f2 _ [] = Just 0
f2 env (x:xs) = case lookup x env of
  Just b -> case f2 env xs of
    Just v -> Just $ b + v
    Nothing -> Nothing
  Nothing -> Nothing

f2' :: (Eq a, Num b) => [(a,b)] -> [a] -> Maybe b
f2' _ [] = Just 0
f2' env (x:xs) = (+) <$> lookup x env <*> f2' env xs