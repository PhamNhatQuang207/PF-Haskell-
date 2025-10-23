import Data.List (sortBy, insertBy)
import Control.Applicative ((<|>))
data ArbreH a = Feuille a | Noeud (ArbreH a) (ArbreH a)
  deriving Show
data Direction = G | D
  deriving Show
type Codes = [Direction]
data Freq a = F a Float
  deriving Show


voyellesH :: ArbreH Char
voyellesH = Noeud (Feuille 'e')
                  (Noeud(Noeud (Noeud (Feuille 'y') (Feuille 'o'))
                    (Feuille 'i'))
                    (Noeud (Feuille 'u') (Feuille 'a')))
voyellesF :: [Freq Char]
voyellesF =
  [F 'a' 17.5,F 'e' 42.2,F 'i' 14.3,F 'o' 10.4,F 'u' 14.7,F 'y' 0.8]
decodeH :: ArbreH a -> Codes -> Maybe(a, Codes)
decodeH (Feuille v) ds = Just (v, ds)
decodeH (Noeud _ _) [] = Nothing
decodeH (Noeud g _) (G:ds) = decodeH g ds
decodeH (Noeud _ d) (D:ds) = decodeH d ds

decodeToutH :: ArbreH a -> Codes -> Maybe [a]
decodeToutH _ [] = Just []
decodeToutH arbre (c:cs) = case decodeH arbre (c:cs) of
  Nothing -> Nothing
  Just (x, reste) -> case decodeToutH arbre reste of
    Nothing -> Nothing
    Just xs -> Just (x:xs)

compareFreq :: Freq a -> Freq a -> Ordering
compareFreq (F _ f1) (F _ f2) = compare f1 f2

sortF :: [Freq a] -> [Freq a]
sortF = sortBy compareFreq

insertF :: Freq a -> [Freq a] -> [Freq a]
insertF = insertBy compareFreq

initH :: [Freq a] -> [Freq (ArbreH a)]
initH = map (\(F v f) -> F (Feuille v) f)

etapeH :: Freq (ArbreH a) -> Freq (ArbreH a) -> [Freq (ArbreH a)] -> [Freq (ArbreH a)]
etapeH (F t1 p1) (F t2 p2) rest =
  let newNode = F (Noeud t1 t2) (p1 + p2)
  in insertF newNode rest

arbreH :: [Freq a] -> Maybe (ArbreH a)
arbreH [] = Nothing 
arbreH freqs = Just (build (sortF (initH freqs)))
  where
    build :: [Freq (ArbreH a)] -> ArbreH a
    build [F finalTree _] = finalTree
    build (t1:t2:rest) = build (etapeH t1 t2 rest)

codeH :: (Eq a) => ArbreH a -> (a -> Maybe Codes)
codeH arbre = \target -> find target arbre
  where
    find :: (Eq a) => a -> ArbreH a -> Maybe Codes
    find target (Feuille v)
      | v == target = Just [] 
      | otherwise   = Nothing
    find target (Noeud g d) =
      (fmap (G:) (find target g))
      <|> (fmap (D:) (find target d))
      <|> (fmap (D:) (find target d))

codeListeH :: (Eq a) => ArbreH a -> ([a] -> Maybe Codes)
codeListeH arbre = \valList ->
  let coderFunc = codeH arbre
  in fmap concat (traverse coderFunc valList)