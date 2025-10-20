import Control.Applicative
type Resultat a = Maybe(a,String)
newtype Parser a = MkParser{runParser :: String -> Resultat a}
evalParser :: Parser a -> String -> Maybe a
evalParser p s = fst <$> runParser p s

instance Functor Parser where
  fmap f p = MkParser $ \s0 ->
                        case runParser p s0 of
                            Just (a,s1) -> Just(f a, s1)
                            Nothing -> Nothing

instance Applicative Parser where
  pure a = MkParser $ \s -> Just (a, s)
  p1 <*> p2 = MkParser $ \s0 ->
                        case runParser p1 s0 of
                          Just (f, s1) -> case runParser p2 s1 of
                                            Just (b, s2) -> Just (f b, s2)
                                            Nothing -> Nothing
instance Alternative Parser where
  empty = MkParser $ const Nothing
  p1 <|> p2 = MkParser $ \s0 ->
                        case runParser p1 s0 of
                          Nothing -> runParser p2 s0
                          r -> r

instance Monad Parser where
  p1 >>= fp2 = MkParser $ \s0 ->
                        case runParser p1 s0 of
                          Just (a, s1) -> runParser (fp2 a) s1
                          Nothing -> Nothing

carQuand :: (Char -> Bool) -> Parser Char
carQuand p = MkParser $ \s ->
                        case s of
                         [] -> Nothing
                         (c:cs) -> if p c
                                   then Just (c, cs)
                                   else Nothing

unCaractereQuelconque :: Parser Char
unCaractereQuelconque = carQuand (const True)

car :: Char -> Parser Char
car c = carQuand (== c)

chaine :: String -> Parser String
chaine = foldr (\c ch -> car c >> ch >>= \s -> return (c:s)) (return "")