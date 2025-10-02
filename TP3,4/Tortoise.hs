-- Tortoise graphics
module Tortoise where
import Graphics.Gloss
import LSysteme
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

-- Exercice 8
type EtatDessin = (EtatTortue, Path)
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
--main = display (InWindow "L-systeme" (1000, 1000) (0, 0)) white dessin

-- Exercise 11 
lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime lsys conf t =
  let n   = floor t               
      mot = lsys !! n            
      d0  = longueurPas conf      
      sc  = facteurEchelle conf   
      dn  = d0 * (sc ^ n)         
      (etat0, _, _, ang, syms) = conf
      confN = (etat0, dn, sc, ang, syms) 
  in interpreteMot confN mot

vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")

main :: IO ()
main = animate
         (InWindow "hilbertAnime" (1000,1000) (0,0)) 
         white 
         hilbertAnime
