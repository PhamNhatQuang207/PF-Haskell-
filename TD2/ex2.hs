import Data.Ord (Down(Down))
import Data.List (sortOn)
import Data.Maybe (mapMaybe)

moyenneCoef :: (Eq a, Fractional a) => [a] -> [a] -> Maybe a
moyenneCoef coef notes  = if totalCoef == 0 then Nothing else Just (totalNotes / totalCoef)
  where
    totalNotes = sum (zipWith (*) notes coef)
    totalCoef = sum coef

moyenneEtudiant :: [(String,[(Double,Double)])] -> [(String, Maybe Double)]
moyenneEtudiant = map (\(nom, notes) -> let (coef,value) = unzip notes
                   in (nom, moyenneCoef coef value))

-- Return only students with a computed average and sort descending
ranking :: [(String,[(Double,Double)])] -> [(String, Double)]
ranking etudiants = let moyennes = moyenneEtudiant etudiants
                        present = mapMaybe (\(n, m) -> fmap (\v -> (n, v)) m) moyennes
                    in sortOn (Down . snd) present

rankingStudentsAverage :: [(String, Double)] -> [(String, Double)]
rankingStudentsAverage xs = sortOn (Down . snd) xs

groupStudentsAverages :: Ord a => a -> a -> [(String, a)] -> [String]
groupStudentsAverages n1 n2 xs =
  let lower = min n1 n2
      upper = max n1 n2
      filtered = filter (\(_, avg) -> avg >= lower && avg <= upper) xs
  in map fst filtered
