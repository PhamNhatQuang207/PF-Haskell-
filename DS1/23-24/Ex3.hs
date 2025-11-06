import Data.List (sortBy)
type Point = (Float, Float) 

comparerPoint :: Point -> Point -> Ordering
comparerPoint (x1,y1) (x2,y2) 
    | x1 < x2 || x1 == x2 && y1 < y2 = LT
    | x1 == x2 && y1 == y2 = EQ
    | otherwise = GT

triPoints :: [Point] -> [Point]
triPoints = sortBy comparerPoint

tourneAGauche :: Point -> Point -> Point -> Bool
tourneAGauche (x1,y1) (x2,y2) (x3,y3) = (x2-x1) * (y3-y1) - (y2 - y1) * (x3 - x1) >= 0

ajouter :: [Point] -> Point -> [Point]
ajouter (b:a:pts) c | tourneAGauche a b c = c:b:a:pts
                    | otherwise = ajouter (a:pts) c
ajouter pts c = c:pts

pBasse :: [Point] -> [Point]
pBasse = foldl ajouter []

pHaute :: [Point] -> [Point]
pHaute = foldr (flip ajouter) []

enveloppe :: [Point] -> [Point]
enveloppe l 
  | length l < 2 = l
  | otherwise = tail hauts ++ tail bas
    where
        pts = triPoints l
        bas = pBasse pts
        hauts = pHaute pts