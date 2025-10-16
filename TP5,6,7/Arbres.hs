import Data.Maybe (isJust)
import Test.QuickCheck
data Arbre coul val = Feuille | Noeud (Arbre coul val) coul val (Arbre coul val)
    deriving (Show, Eq)
data Coul = Rouge | Noir
    deriving (Show, Eq)
mapArbre :: (coul1 -> coul2) -> (val1 -> val2) -> Arbre coul1 val1 -> Arbre coul2 val2
mapArbre _ _ Feuille = Feuille
mapArbre f g (Noeud ag c v ad) = Noeud (mapArbre f g ag) (f c) (g v) (mapArbre f g ad)

hauteur :: Arbre coul val -> Int
hauteur Feuille = 0
hauteur (Noeud g _ _ d) = 1 + max (hauteur g) (hauteur d)

taille :: Arbre coul val -> Int
taille Feuille = 0
taille (Noeud g _ _ d) = 1 + taille g + taille d

dimension :: (Int -> Int -> Int) -> Int -> Arbre coul val -> Int
dimension _ leafValue Feuille = leafValue
dimension nodeOp leafValue (Noeud g _ _ d) = 1 + nodeOp (dimension nodeOp leafValue g) (dimension nodeOp leafValue d)

hauteur' :: Arbre coul val -> Int
hauteur' = dimension max 0

taille' :: Arbre coul val -> Int
taille' = dimension (+) 0

peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche [] = Feuille
peigneGauche ((c,a):xs) = Noeud (peigneGauche xs) c a Feuille

estParfait :: Arbre c a -> Bool
estParfait = isJust . check
    where
        check :: Arbre c a -> Maybe Int
        check Feuille = Just 0 
        check (Noeud g _ _ d) =
            case (check g, check d) of
                (Just hg, Just hd) | hg == hd -> Just (1 + hg)
                _ -> Nothing

foldArbre :: (b -> b -> b) -> b -> Arbre c a -> b
foldArbre _ leafCase Feuille = leafCase
foldArbre nodeCase leafCase (Noeud g _ _ d) =
    nodeCase (foldArbre nodeCase leafCase g) (foldArbre nodeCase leafCase d)

taille'' :: Arbre c a -> Int
taille'' = foldArbre (\leftSize rightSize -> 1 + leftSize + rightSize) 0

hauteur'' :: Arbre c a -> Int
hauteur'' = foldArbre (\leftHeight rightHeight -> 1 + max leftHeight rightHeight) 0

estParfait' :: Arbre c a -> Bool
estParfait' = isJust . foldArbre nodeCase (Just 0)
    where
        nodeCase :: Maybe Int -> Maybe Int -> Maybe Int
        nodeCase (Just h1) (Just h2) | h1 == h2 = Just (1 + h1)
        nodeCase _ _ = Nothing  
        

isLeftComb :: Arbre c a -> Bool
isLeftComb Feuille = True
isLeftComb (Noeud g _ _ Feuille) = isLeftComb g 
isLeftComb _ = False


parfait :: Int -> [(c, a)] -> Arbre c a
parfait 0 _ = Feuille
parfait n xs = Noeud (parfait (n-1) gauche) c a (parfait (n-1) droite)
  where
    (gauche, (c, a):droite) = splitAt (length xs `div` 2) xs

repete :: a -> [a]
repete x = x : repete x

repete' :: a -> [a]
repete' x = iterate id x

listeTuples :: [ ((), Char) ]
listeTuples = zip (repeat ()) ['a'..]

aplatit :: Arbre c a -> [(c, a)]
aplatit Feuille = []
aplatit (Noeud g c a d) = aplatit g ++ [(c, a)] ++ aplatit d

element :: Eq a => a -> Arbre c a -> Bool
element _ Feuille = False
element x (Noeud g c a d)
    | x `elem` l = True
    | otherwise = False
    where
        l = aplatit (Noeud g c a d)

-- Examples
f :: Coul -> [Coul]
f c = [c,c]
g :: Int -> Float
g v = (fromIntegral v) / 2.0
monArbre :: Arbre Coul Int
monArbre = Noeud (Noeud Feuille Rouge 5 Feuille) Noir 10 (Noeud Feuille Rouge 20 (Noeud Feuille Noir 25 Feuille))
parfaitArbre :: Arbre Coul Int
parfaitArbre = Noeud (Noeud Feuille Rouge 1 Feuille) Noir 2 (Noeud Feuille Rouge 3 Feuille)

listArbre = [((),'a'),((),'b'),((),'c')]

-- Test QuickCheck
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)

type C = ()
type A = Int

instance (Arbitrary c, Arbitrary a) => Arbitrary (Arbre c a) where
  arbitrary = sized createTree
    where
      createTree n
        | n <= 0    = return Feuille
        | otherwise = frequency
            [ (1, return Feuille)
            , (4, Noeud <$> resize (n `div` 2) arbitrary 
                         <*> arbitrary 
                         <*> arbitrary 
                         <*> resize (n `div` 2) arbitrary) 
            ]

prop_perfectCombIsTrivial :: Arbre C A -> Property
prop_perfectCombIsTrivial tree =
    (estParfait tree && isLeftComb tree) ==> (hauteur tree <= 1)

parfait4 = parfait 4 (take 15 listeTuples) 
