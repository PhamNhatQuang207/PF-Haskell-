import Parser
type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)


espacesP :: Parser ()
espacesP = many (car ' ') >> pure ()

nomP :: Parser Nom
nomP = some (carQuand estLettre) >>= \cs -> espacesP >> pure cs
    where estLettre c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

varP :: Parser Expression
varP = nomP >>= \n -> pure (Var n)

applique ::[Expression] -> Expression
applique = foldl1 App

exprP :: Parser Expression
exprP = exprParentheseeP <|> lambdaP <|> varP

exprsP :: Parser Expression
exprsP = some exprP >>= \es -> pure (applique es)

lambdaP :: Parser Expression
lambdaP = do
    car '\\'    >> espacesP
    x  <- nomP
    car '-'     >> espacesP
    car '>'     >> espacesP
    e  <- exprsP
    pure (Lam x e)

exprParentheseeP :: Parser Expression
exprParentheseeP = do
    car '('     >> espacesP
    e  <- exprsP
    car ')'     >> espacesP
    pure e

nombreP :: Parser Expression
nombreP = some (carQuand estChiffre) >>= \cs -> espacesP >>
           pure (Lit (Entier (lireEntier cs)))
    where estChiffre c = c >= '0' && c <= '9'
          lireEntier cs = read cs :: Integer

boolP :: Parser Expression
boolP = (chaine "True"  >> espacesP >> pure (Lit (Bool True)))
    <|> (chaine "False" >> espacesP >> pure (Lit (Bool False)))

expressionP :: Parser Expression
expressionP = espacesP >> (boolP <|> nombreP <|> exprsP)

ras :: String -> Expression
ras s = resultat (runParser expressionP s)
    where resultat (Just (r, "")) = r
          resultat _               = error "Error analyse syntax"