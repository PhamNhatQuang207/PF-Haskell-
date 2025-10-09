module Ex1 where
data Formule = Var String          
             | Vrai                
             | Faux                
             | Et Formule Formule  
             | Ou Formule Formule  
             | Non Formule         
             deriving (Show, Eq , Ord) 

foldFormule :: (String -> a) -> a -> a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> Formule -> a
foldFormule fVar fVrai fFaux fEt fOu fNon formule =
  case formule of
    Var s       -> fVar s
    Vrai        -> fVrai
    Faux        -> fFaux
    Et f1 f2    -> fEt (rec f1) (rec f2)
    Ou f1 f2    -> fOu (rec f1) (rec f2)
    Non f       -> fNon (rec f)
  where
    rec = foldFormule fVar fVrai fFaux fEt fOu fNon