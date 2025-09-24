data ABR a = N a (ABR a) (ABR a) | L
    deriving (Show, Eq)

-- Q2.1
estElement :: Ord a => a -> ABR a -> Bool
estElement _ L = False
estElement x (N a t1 t2)
    | x == a = True
    | x < a = estElement x t1
    | x > a = estElement x t2

-- Q2.2
assoc :: Ord a => a -> ABR (a,b) -> Maybe b
assoc _ L = Nothing
assoc x (N (a,b) t1 t2)
    | x == a    = Just b
    | x < a     = assoc x t1
    | otherwise = assoc x t2


-- Q2.3
abrToList :: ABR a -> [a]
abrToList L = []
abrToList (N a t1 t2) = abrToList t1 ++ [a] ++ abrToList t2

estOrdonnee :: Ord a => [a] -> Bool
estOrdonnee []       = True
estOrdonnee [_]      = True
estOrdonnee (x:y:xs) = x < y && estOrdonnee (y:xs)

estABR :: Ord a => ABR a -> Bool
estABR t = estOrdonnee (abrToList t)

-- Q2.4
estEquilibre :: ABR a -> Bool
estEquilibre t = case hauteur t of
    Nothing -> False
    Just _  -> True
  where
    hauteur :: ABR a -> Maybe Int
    hauteur L = Just 0
    hauteur (N _ t1 t2) =
        case (hauteur t1, hauteur t2) of
            (Just h1, Just h2)
                | abs (h1 - h2) <= 1 -> Just (1 + max h1 h2)
                | otherwise          -> Nothing
            _ -> Nothing

-- Q2.5
data Equilibre = Gauche | Droit | Egal
    deriving (Show, Eq)

data AVL a = Noeud a Equilibre (AVL a) (AVL a) | Feuille
    deriving (Show, Eq)

-- Examples
-- Tree:      5
--          /   \
--         3     8
--        / \   /
--       1   4 6
t1 :: ABR Int
t1 = N 5 (N 3 (N 1 L L) (N 4 L L))
         (N 8 (N 6 L L) L)


-- Tree dict: key -> value
--        (5,"a")
--        /     \
--   (3,"b")   (8,"c") 
dict :: ABR (Int, String)
dict = N (5,"a")
         (N (3,"b") L L)
         (N (8,"c") L L)

t2 :: ABR Integer
t2 = N 5 (N 3 L L) (N 8 L L)

t3 :: ABR Integer
t3 = N 5 (N 7 (N 4 L L) L) L

avl1 :: AVL Int
avl1 = Noeud 5 Egal
           (Noeud 3 Egal Feuille Feuille)
           (Noeud 8 Egal Feuille Feuille)