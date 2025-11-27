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
exprP = exprParentheseeP <|> lambdaP <|> varP <|> nombreP <|> boolP

exprsP :: Parser Expression
exprsP = some exprP >>= \es -> pure (applique es)

lambdaP :: Parser Expression
lambdaP = do
    car '\\'    >> espacesP
    x  <- nomP
    chaine "->" >> espacesP
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
expressionP = espacesP >> exprsP

ras :: String -> Expression
ras s = resultat (runParser expressionP s)

data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)

instance Show ValeurA where
    show (VFonctionA _) = "λ"
    show (VLitteralA (Bool True)) = "True"
    show (VLitteralA (Bool False)) = "False"
    show (VLitteralA (Entier a)) = show a

type Environnement a = [(Nom, a)]

interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA env (Lit l) = VLitteralA l
interpreteA env (Var n) = case lookup n env of
                             Just v  -> v
                             Nothing -> error ("Variable non définie: " ++ n)
interpreteA env (Lam n e) = VFonctionA (\v -> interpreteA ((n,v):env) e)
interpreteA env (App e1 e2) =
    case interpreteA env e1 of
        VFonctionA f -> f (interpreteA env e2)
        _            -> error "Tentative d'application d'une valeur non fonctionnelle"
        
