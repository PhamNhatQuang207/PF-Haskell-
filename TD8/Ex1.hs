import Parser
import Data.Char (isSpace, isDigit)
import Control.Applicative (Alternative(many))
data JSON = JsonNull -- Valeur null
        | JsonBool Bool -- Booléens
        | JsonInt Int -- Entiers
        | JsonFloat Float -- Flottants
        | JsonString String -- Chaînes de caractères
        | JsonArray [JSON] -- Liste
        | JsonObject [(String, JSON)] -- Objets
        deriving Show
espacesP :: Parser ()
espacesP = many (car ' ') >> pure ()

espaceCP :: Parser Int
espaceCP = many (car ' ') >>= \cs -> pure (length cs)

passeBlancs :: Parser a -> Parser a
passeBlancs p = p >>= \v -> many (carQuand isSpace) >>= \s -> pure v 

passeNULL :: Parser JSON
passeNULL = chaine "null" >> pure JsonNull

passeBool :: Parser JSON
passeBool = (chaine "true" >> pure (JsonBool True)) <|>
            (chaine "false" >> pure (JsonBool False))

toutSauf :: [Char] -> Parser Char
toutSauf cs = carQuand (\c -> not (elem c cs))

chaineP :: Parser String
chaineP = car '"' >> many (toutSauf ['"']) >>= \s -> car '"' >> pure s

parseChaine :: Parser JSON
parseChaine = do
    s <- chaineP
    pure (JsonString s)

parseChaineM :: Parser JSON
parseChaineM = chaineP >>= \s -> pure (JsonString s)

parseChaineA :: Parser JSON
parseChaineA = JsonString <$> chaineP

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
p1 <++> p2 = (++) <$> p1 <*> p2

chaineOption:: String -> Parser String
chaineOption s = (chaine s) <|> pure ""

nombre :: Parser String
nombre = some (carQuand isDigit)

passeInt :: Parser JSON
passeInt =  passeBlancs aux
    where
        aux = do
            s <- chaineOption "-"
            n <- nombre
            pure (JsonInt (read (s ++ n)))