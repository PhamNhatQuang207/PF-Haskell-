-- Implement L-systems
module LSysteme where
type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

-- Exercice 1
motSuivant :: Regles -> Mot -> Mot
motSuivant r m = concatMap r m

motSuivant' :: Regles -> Mot -> Mot
motSuivant' r []     = []
motSuivant' r (x:xs) = r x ++ motSuivant' r xs

motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' r mot = [ y | x <- mot, y <- r x ]

-- Exercice 2
reglesKoch :: Regles
reglesKoch 'F' = "F-F++F-F"
reglesKoch '+' = "+"
reglesKoch '-' = "-"
reglesKoch  c  = [c]   


axiomeKoch :: Axiome
axiomeKoch = "F"

koch :: LSysteme
koch = lsysteme axiomeKoch reglesKoch

-- Exercice 3
lsysteme :: Axiome -> Regles -> LSysteme
lsysteme u0 r = iterate (motSuivant r) u0

