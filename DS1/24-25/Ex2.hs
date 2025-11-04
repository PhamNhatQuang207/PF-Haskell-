import Data.List (sortOn)
type Equipe = String
type ResultatsRugby = [(Equipe, Int)]
data Match = Match
 {  equipe1 :: Equipe, -- Équipe qui reçoit
    equipe2 :: Equipe, -- Équipe qui se déplace
    essai1 :: Int, -- Nombre d'essais inscrits par l'equipe qui reçoit à la fin du match
    score1 :: Int, -- Score de l'equipe qui reçoit à la fin du match
    essai2 :: Int, -- Nombre d'essais inscrits par l'equipe qui se déplace à la fin du match
    score2 :: Int -- Score de l'equipe qui se déplace à la fin du match
 }
type Journee = [Match]
type Championnat = [Journee]

grenoble :: Equipe
grenoble = "FC Grenoble Amazones"
romagnat :: Equipe
romagnat = "ASN Romagnat Rugby Feminin"
toulouse :: Equipe
toulouse = "Stade Toulousain Rugby"
villeneuve :: Equipe
villeneuve = "Stade Villeneuvois Lille Metropole"

m11 :: Match
m11 = Match villeneuve romagnat 4 34 2 27
m12 :: Match
m12 = Match toulouse grenoble 5 25 2 21
m21 :: Match
m21 = Match grenoble villeneuve 4 31 1 19
m22 :: Match
m22 = Match romagnat toulouse 0 6 2 20

j1 :: Journee
j1 = [m11, m12]
j2 :: Journee
j2 = [m21, m22]
champ :: Championnat
champ = [j1, j2]

resultatMatch :: Match -> ResultatsRugby
resultatMatch m = [(equipe1 m,pt1 + bO1 + bD1),(equipe2 m, pt2 +bO2 + bD2)]
    where
        (pt1,pt2) | score1 m > score2 m = (4,0)
                  | score1 m < score2 m = (0,4)
                  | otherwise = (2,2)
        (bO1,bO2) | essai1 m - essai2 m >= 3 = (1,0)
                  | essai2 m - essai1 m >= 3 = (0,1)
                  | otherwise = (0,0)
        (bD1,bD2) | score1 m > score2 m && score1 m - score2 m <= 5 = (0,1)
                  | score2 m > score1 m && score2 m - score1 m <= 5 = (1,0)
                  | otherwise = (0,0)

resultatsJournee :: Journee -> ResultatsRugby
resultatsJournee = sortOn (\(x,_) -> x) . concatMap resultatMatch 

combineResultats :: ResultatsRugby -> ResultatsRugby -> ResultatsRugby
combineResultats = zipWith (\(a,i1) (_,i2) -> (a,i1+i2) ) 

resultatsChampionnat :: Championnat -> ResultatsRugby
resultatsChampionnat (j:l) = foldl (\acc a ->combineResultats acc (resultatsJournee a) ) (resultatsJournee j) l

classementChampionnat :: Championnat -> ResultatsRugby
classementChampionnat = sortOn (\(_,b) -> -b) . resultatsChampionnat

showClassement :: Championnat -> String
showClassement c = unlines ( zipWith(\ rg (e, pt) -> show rg ++ ". " ++ e ++ " -- " ++ show pt) [1 .. ] (classementChampionnat c))
