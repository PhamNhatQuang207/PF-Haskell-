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
estParfait t = case hauteur t of
    0 -> True
    h -> taille t == 2^h - 1

-- Examples
f :: Coul -> [Coul]
f c = [c,c]
g :: Int -> Float
g v = (fromIntegral v) / 2.0
monArbre :: Arbre Coul Int
monArbre = Noeud (Noeud Feuille Rouge 5 Feuille) Noir 10 (Noeud Feuille Rouge 20 (Noeud Feuille Noir 25 Feuille))
parfaitArbre :: Arbre Coul Int
parfaitArbre = Noeud (Noeud Feuille Rouge 1 Feuille) Noir 2 (Noeud Feuille Rouge 3 Feuille)