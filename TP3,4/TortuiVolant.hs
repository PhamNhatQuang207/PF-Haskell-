-- TortuiVolant.hs
-- Flying turtle implementation for L-systems with Gloss
-- Adds support for "[" (push) and "]" (pop) to simulate branching plants

module TortuiVolant where

import Tortoise hiding (EtatDessin, interpreteSymbole, interpreteMot, main)
import LSysteme
import Graphics.Gloss

-- EtatDessin now contains:
-- * a stack of turtle states (head = current state)
-- * a list of paths (each Path is a list of Points). The head of paths is the current path.
type EtatDessin = ([EtatTortue], [Path])

-- Initialize the drawing state:
-- start with the initial turtle state, and one path that contains the starting point
etatInitialDessin :: Config -> EtatDessin
etatInitialDessin conf =
  ([etatInitial conf], [[fst (etatInitial conf)]])

-- Interpret one symbol and update the drawing state.
-- Note: we pattern-match the EtatDessin as (etatCourant : stack, courant : rest)
-- so we know there is at least one state and at least one current path.

interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole conf (etatCourant:stack, courant:rest) sym =
  case sym of
    -- Move forward: compute new position and append it to the current path
    'F' ->
      let ((x,y), cap) = etatCourant
          d             = longueurPas conf
          x'            = x + d * cos cap
          y'            = y + d * sin cap
          newEtat       = ((x', y'), cap)
          newCourant    = if null courant 
                          then [(x, y), (x', y')] 
                          else courant ++ [(x', y')]
          newPaths      = newCourant : rest
      in (newEtat : stack, newPaths)

    -- Turn left: update heading, path unchanged
    '+' ->
      let ((x,y), cap) = etatCourant
          newEtat      = ((x,y), cap + angle conf)
      in (newEtat : stack, courant : rest)

    -- Turn right: update heading, path unchanged
    '-' ->
      let ((x,y), cap) = etatCourant
          newEtat      = ((x,y), cap - angle conf)
      in (newEtat : stack, courant : rest)

    -- Push: save the current state and start a new path for the branch
    '[' ->
      (etatCourant : etatCourant : stack, [] : courant : rest)

    -- Pop: restore the saved state from stack
    ']' ->
      case stack of
        (saved:restStack) -> (saved : restStack, [] : courant : rest)
        -- Empty stack (shouldn't happen)
        [] -> (etatCourant : stack, courant : rest)
        
    -- Unknown symbol: do nothing
    _ -> (etatCourant : stack, courant : rest)


-- Interpret a whole word into a Gloss Picture: combine all paths
interpreteMot :: Config -> Mot -> Picture
interpreteMot conf mot =
  let etat0 = etatInitialDessin conf
      (_, chemins) = foldl (interpreteSymbole conf) etat0 mot
      -- Filter out empty paths and convert each path to a Line
      validPaths = filter (not . null) chemins
  in Pictures (map Line validPaths)

-------------------------------------------------
-- Example L-systems for plants
-------------------------------------------------

brindille :: LSysteme
brindille = lsysteme "F" regles
    where regles 'F' = "F[-F]F[+F]F"
          regles  s  = [s]

broussaille :: LSysteme
broussaille = lsysteme "F" regles
    where regles 'F' = "FF-[-F+F+F]+[+F-F-F]"
          regles  s  = [s]

brindilleAnime :: Float -> Picture
brindilleAnime = lsystemeAnime brindille (((0, -400), pi/2), 800, 1/3, 25*pi/180, "F+-[]")

broussailleAnime :: Float -> Picture
broussailleAnime = lsystemeAnime broussaille (((0, -400), pi/2), 500, 2/5, 25*pi/180, "F+-[]")

-------------------------------------------------
-- Main: animate one of the examples
-------------------------------------------------

main :: IO ()
main = animate (InWindow "Botanist" (1000,1000) (0,0)) white brindilleAnime
