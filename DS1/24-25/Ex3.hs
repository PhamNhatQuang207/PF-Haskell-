data Couleur = Blanc | Noir
data Arbre a = Feuille | Noeud Couleur a (Arbre a) (Arbre a)

foldArbre :: b -> (Couleur -> a -> b -> b -> b) -> Arbre a -> b
foldArbre f _ Feuille = f
foldArbre f noeud (Noeud c a t1 t2) = noeud c a (g t1) (g t2)
        where
         g = foldArbre f noeud

arbres1 :: Arbre Int
arbres1 = Noeud Noir 3 (Noeud Noir 2 (Noeud Blanc 1 Feuille Feuille) Feuille ) (Noeud Noir 4 Feuille Feuille)

arbres2 :: Arbre Int
arbres2 = Noeud Noir 3 (Noeud Noir 2 (Noeud Noir 1 Feuille Feuille) Feuille ) (Noeud Noir 4 Feuille Feuille)

arbres3 :: Arbre Int
arbres3 = Noeud Noir 3 (Noeud Blanc 2 (Noeud Blanc 1 Feuille Feuille) Feuille ) (Noeud Blanc 4 Feuille Feuille)

arbres4 :: Arbre Int
arbres4 = Noeud Noir 2 (Noeud Noir 4 (Noeud Blanc 1 Feuille Feuille) Feuille ) (Noeud Noir 5 Feuille Feuille)

arbres5 :: Arbre Int
arbres5 = Noeud Noir 2 (Noeud Noir 2 (Noeud Blanc 1 Feuille Feuille) Feuille ) (Noeud Noir 5 Feuille Feuille)

arbres6 :: Arbre Int
arbres6 = Noeud Noir 3 (Noeud Noir 2 (Noeud Blanc 1 Feuille Feuille) Feuille ) (Noeud Noir 0 Feuille Feuille)

elements :: Arbre a -> [a]
elements Feuille = []
elements (Noeud _ a t1 t2) = elements t1 ++ [a] ++ elements t2

elements' :: Arbre a -> [a]
elements' = foldArbre [] (\_ a t1 t2 -> t1 ++ [a] ++ t2 )

listeOrd :: Ord a => [a] -> Bool
listeOrd [x] = True
listeOrd [] = False
listeOrd (x:y:xs) = (x < y) && listeOrd (y:xs) 

arbreOrd :: Ord a => Arbre a -> Bool
arbreOrd = listeOrd . elements

bienColore :: Arbre a -> Maybe (Couleur,Int)
bienColore Feuille = Just (Noir,0)
bienColore (Noeud c _ t1 t2) = case (bienColore t1, bienColore t2) of
                                (Just (c1,n1), Just (c2,n2)) ->
                                    (if n1==n2
                                        then case (c,c1,c2) of
                                            (Blanc,_,Blanc) -> Nothing
                                            (Blanc,Blanc,_) -> Nothing
                                            (Blanc, _, _ ) -> Just (c, n1)
                                            _ -> Just (c,n1+1)
                                    else Nothing)
                                _ -> Nothing

arbreCorrect :: Ord a => Arbre a -> Bool
arbreCorrect a = arbreOrd a && (case bienColore a of
                                    Just (Noir,_) -> True
                                    _ -> False)

appartient :: Ord t => t -> Arbre t -> Bool
appartient a (Noeud _ a' t1 t2) | a==a' = True
                                | a < a' = appartient a t1
                                | otherwise = appartient a t2
appartient _ _ = False

equilibre :: Arbre a -> Arbre a
equilibre t = case t of
                Noeud Noir z (Noeud Blanc y (Noeud Blanc x a b) c) d -> mkEq x y z a b c d
                Noeud Noir z (Noeud Blanc x a (Noeud Blanc y b c)) d -> mkEq x y z a b c d
                Noeud Noir x a (Noeud Blanc z (Noeud Blanc y b c) d) -> mkEq x y z a b c d
                Noeud Noir x a (Noeud Blanc y b (Noeud Blanc z c d)) -> mkEq x y z a b c d
                _ -> t
            where
                mkEq x y z a b c d = Noeud Blanc y (Noeud Noir x a b) (Noeud Noir z c d)

insererAux :: Ord a => a -> Arbre a -> Arbre a
insererAux a Feuille = Noeud Blanc a Feuille Feuille
insererAux a t@(Noeud c x t1 t2) | a < x = equilibre (Noeud c x (insererAux a t1) t2)
                                 | a > x = equilibre (Noeud c x t1 (insererAux a t2))
                                 | otherwise = t

inserer :: Ord a => a -> Arbre a -> Arbre a
inserer a t = case insererAux a t of
                Noeud Blanc x t1 t2 -> Noeud Noir x t1 t2
                t' -> t'

insererAuxM :: Ord a => a -> Arbre a -> Maybe (Arbre a)
insererAuxM a Feuille = Just $ Noeud Blanc a Feuille Feuille
insererAuxM a (Noeud c x t1 t2) | a < x = case insererAuxM a t1 of
                                        Just t1' -> Just $ equilibre(Noeud c x t1' t2)
                                        _ -> Nothing
                                | a > x = case insererAuxM a t2 of
                                        Just t2' -> Just$ equilibre (Noeud c x t1 t2')
                                        _ -> Nothing
                                | otherwise = Nothing

insererM :: Ord a => a -> Arbre a -> Maybe (Arbre a)
insererM a t = case insererAuxM a t of
                Just (Noeud Blanc x t1 t2) -> Just(Noeud Noir x t1 t2)
                t' -> t'