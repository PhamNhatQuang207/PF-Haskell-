import Parser
import Data.Char (isDigit)
import Control.Applicative (Alternative(many,some))

chiffre :: Parser Char
chiffre = carQuand isDigit

entier :: Parser Int
entier = read <$> some chiffre

entierRelatif :: Parser Int
entierRelatif = negatif <|> entier
    where
        negatif = do
            car '-'
            n <- entier
            return (-n)