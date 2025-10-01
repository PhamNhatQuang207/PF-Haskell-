import Data.Ord (Down(Down))
import Data.List (sortOn)
import Data.Maybe (mapMaybe)

moyenneCoef :: Fractional a => [a] -> a -> [a] -> a
moyenneCoef coef t notes = sum (zipWith (*) coef notes) / t 

moyenneEtu :: Fractional b => [b] -> [(String, [b])] -> [(String,b)]
moyenneEtu coef  = map f  
    where
        f (nom,notes) = (nom,moyenneCoef coef t notes)
        t = sum coef

ranking :: Ord a => [(String,a)] -> [(String,a)]
ranking  = sortOn (Down . snd)

groupeEtudiantsNotes :: Ord a => a -> a ->[(String,a)] -> [(String,a)]
groupeEtudiantsNotes a b = filter (\(_,notes) -> notes <= max a b && notes >= min a b)