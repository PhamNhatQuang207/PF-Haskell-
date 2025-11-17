-- |
-- Module      :  Parser


module Parser ( Parser
              , Resultat
              , runParser
              , resultat
              , unCaractereQuelconque
              , carQuand
              , car
              , chaine
              , (<|>), empty, many, some )
where

import Control.Applicative
import Control.Monad (ap, liftM)


type Resultat a = Maybe (a, String)
data Parser a   = MkParser (String -> Resultat a)


runParser :: Parser a -> String -> Resultat a
runParser (MkParser f) = f

resultat :: Resultat a -> a
resultat (Just (r, _)) = r





unCaractereQuelconque :: Parser Char
unCaractereQuelconque = MkParser f
    where f     "" = Nothing
          f (c:cs) = Just (c, cs)

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure v = MkParser f
        where f cs = Just (v, cs)

    (<*>) = ap

instance Alternative Parser where
    empty = MkParser (const Nothing)

    p1 <|> p2 = MkParser f
        where f cs = case runParser p1 cs of
                        Nothing -> runParser p2 cs
                        r       -> r
    p1 <|> p2 = MkParser f
        where f cs = case runParser p1 cs of
                        Nothing -> runParser p2 cs
                        r       -> r
    some :: Parser a -> Parser [a]
    some p = do r  <- p
                rs <- many p
                pure (r:rs)

instance Monad Parser where
    p >>= fp = MkParser f
        where f cs = case runParser p cs of
                        Nothing       -> Nothing
                        Just (r, cs') -> runParser (fp r) cs'



carQuand :: (Char -> Bool) -> Parser Char
carQuand cond = unCaractereQuelconque >>= filtre
    where filtre c | cond c    = pure c
                   | otherwise = empty

car :: Char -> Parser Char
car c = carQuand (c ==)

chaine :: String -> Parser String
chaine     "" = pure ""
chaine (c:cs) = car c     >>
                chaine cs >>
                pure (c:cs)