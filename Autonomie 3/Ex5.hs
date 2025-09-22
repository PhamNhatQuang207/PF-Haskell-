data Formule
    = Var String
    | Non Formule
    | Et Formule Formule
    | Ou Formule Formule
    deriving (Show, Eq)


evalFormule :: [(String, Bool)] -> Formule -> Maybe Bool
evalFormule env f = case f of
    Var x ->
        lookup x env
    Non f1 -> case evalFormule env f1 of
        Just v  -> Just (not v)
        Nothing -> Nothing
    Et f1 f2 -> case (evalFormule env f1, evalFormule env f2) of
        (Just v1, Just v2) -> Just (v1 && v2)
        _                  -> Nothing
    Ou f1 f2 -> case (evalFormule env f1, evalFormule env f2) of
        (Just v1, Just v2) -> Just (v1 || v2)
        _                  -> Nothing
