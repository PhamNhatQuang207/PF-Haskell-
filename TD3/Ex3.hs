data Section = Section String [Section]
    deriving (Show)
type Sommaire = [Section]
lyah :: Sommaire
lyah = [Section "Introduction" [
            Section "About this tutorial" [],
            Section "So what's Haskell?" [],
            Section "What you need to dive in" []],
        Section "Starting Out" [
            Section "Ready, set, go!" [],
            Section "Baby's first functions" [],
            Section "An intro to lists" [],
            Section "Texas ranges" [],
            Section "I'm a list comprehension" [],
            Section "Tuples" []]]

test :: Section
test =  Section "Introduction" [
            Section "About this tutorial" [],
            Section "So what's Haskell?" [],
            Section "What you need to dive in" []]

-- Q3.1
sommaireToStrings :: Sommaire -> [String]
sommaireToStrings s = concatMap sectionToStrings s

sectionToStrings :: Section -> [String]
sectionToStrings (Section titre sousSections) = titre : concatMap (map (".." ++).sectionToStrings) sousSections

-- Q3.2
numSommaire :: String -> Sommaire -> Sommaire
numSommaire s som = zipWith (\i sec -> numSection s [i] sec) [1..] som


numSection :: String -> [Int] -> Section -> Section
numSection s (n:ns) (Section titre sousSections) = 
    let numStr = concatMap (\x -> show x ++ ".")(n:ns)
        nouveauTitre = s ++ numStr ++ " " ++ titre
        nouveauxSection = zipWith (\i sec -> numSection (".." ++ s) (n:ns++[i]) sec ) [1..] sousSections
    in Section nouveauTitre nouveauxSection

-- Q3.3
showSommaire :: Sommaire -> String
showSommaire som = unlines (sommaireToStrings(numSommaire "" som))
