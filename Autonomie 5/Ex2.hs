f1 :: Num a => Either String a -> Either String a -> Either String a -> Either String a
f1 e1 e2 e3 = case e1 of
                Right v1 -> case e2 of
                            Right v2 -> case e3 of
                                        Right v3 -> Right $ (v1 + v2) * v3
                                        l -> l
                            l -> l
                l -> l

f1' :: Num a => Either String a -> Either String a -> Either String a -> Either String a
f1' e1 e2 e3 = (\v1 v2 v3 -> (v1 + v2) * v3) <$> e1 <*> e2 <*> e3

f2 :: (Num a, Show a) => (a->Bool) -> [a] -> Either String a
f2 p = foldr f (Right 0)
    where
        f a (Right v) | p a = Right (a + v)
                      | otherwise = Left ("Valeur " ++ show a ++ " non valide")
        f _ l = l

f2_applicative :: (Num a, Show a) => (a -> Bool) -> [a] -> Either String a
f2_applicative p = foldl (\acc x -> (+) <$> acc <*> check x) (Right 0)
  where
    check a = if p a then Right a else Left $ show a ++ " ne verifie pas la propriete"


