-- Tortoise graphics
import Graphics.Gloss
type Symbole = Char
type Mot = [Symbole]
type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

-- Exercice 4
etatInitial :: Config -> EtatTortue
etatInitial (etat, _, _, _, _) = etat

longueurPas :: Config -> Float
longueurPas (_, d, _, _, _) = d

facteurEchelle :: Config -> Float
facteurEchelle (_, _, e, _, _) = e

angle :: Config -> Float
angle (_, _, _, a, _) = a

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_, _, _, _, s) = s

-- Exercice 5
avance :: Config -> EtatTortue -> EtatTortue
avance conf ((x,y), cap) =
  let d = longueurPas conf
      x' = x + d * cos cap
      y' = y + d * sin cap
  in ((x', y'), cap)

-- Exercice 6
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche conf (pt, cap) =
  let a = angle conf
  in (pt, cap + a)

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite conf (pt, cap) =
  let a = angle conf
  in (pt, cap - a)

-- Exercice 7
filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue conf mot =
  let cmds = symbolesTortue conf
  in [c | c <- mot, c `elem` cmds]

type EtatDessin = (EtatTortue, Path)

-- Exercice 8
interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole conf (etat, path) s =
  case s of
    'F' ->
      let newEtat@(newPos, _) = avance conf etat
      in (newEtat, newPos : path)  
    '+' ->
      let newEtat = tourneAGauche conf etat
      in (newEtat, path)
    '-' ->
      let newEtat = tourneADroite conf etat
      in (newEtat, path)
    _ -> (etat, path)

-- Exercice 10
interpreteMot :: Config -> Mot -> Picture
interpreteMot conf mot =
  let 
      initEtatDessin = (etatInitial conf, [fst (etatInitial conf)])
      finalEtatDessin = foldl (interpreteSymbole conf) initEtatDessin mot
      path = snd finalEtatDessin
  in line (reverse path)   

-- Exemple de dessin
dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"
main = display (InWindow "L-système" (1000, 1000) (0, 0)) white dessin