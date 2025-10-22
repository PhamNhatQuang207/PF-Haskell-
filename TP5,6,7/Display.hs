import Arbres
import Control.Concurrent (threadDelay)

type ArbreRN a = Arbre Coul a
noeud :: (c -> String) -> (a -> String) -> (c, a) -> String
noeud fnCouleur fnValeur (c, a) =
    let
        -- 1. Convert the value to a String (this is the node's ID)
        valStr = fnValeur a
        -- 2. Convert the color to a String (this is the dot color name)
        colStr = fnCouleur c
    in
        -- 3. Combine them into the final dot syntax line
        valStr ++ " [color=" ++ colStr ++ ", fontcolor=" ++ colStr ++ "]"

arcs :: Arbre c a -> [(a, a)]
arcs Feuille = []
arcs (Noeud g _ v d) =
    let subArcs = arcs g ++ arcs d
        leftArc = case g of
                    Noeud _ _ vg _ -> [(v, vg)]
                    Feuille        -> []
        rightArc = case d of
                     Noeud _ _ vd _ -> [(v, vd)]
                     Feuille        -> []
    in
    leftArc ++ rightArc ++ subArcs

arc :: (a -> String) -> (a, a) -> String
arc fnValeur (parent, child) =
    fnValeur parent ++ " -> " ++ fnValeur child

dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise nom fnCouleur fnValeur tree =
    let
        -- 1. Get the list of node data, e.g., [(Rouge, 5), (Noir, 10), ...]
        nodeData = aplatit tree
        -- 2. Get the list of arc data, e.g., [(10, 5), (10, 20), ...]
        arcData = arcs tree
        -- 3. Convert all node data to formatted dot strings
        -- e.g., "5 [color=red, fontcolor=red]"
        nodeLines = map (noeud fnCouleur fnValeur) nodeData
        -- 4. Convert all arc data to formatted dot strings
        -- e.g., "10 -> 5"
        arcLines = map (arc fnValeur) arcData
    in
        -- 5. Assemble all parts into one string, with newlines between each line
        unlines $
          [ "digraph \"" ++ nom ++ "\" {"
          , "    node [fontname=\"DejaVu-Sans\", shape=circle]"
          , ""
          , "    /* Liste des nœuds */"
          ]
          ++ map ("    " ++) nodeLines  -- Add indentation to each node line
          ++
          [ ""
          , "    /* Liste des arcs */"
          ]
          ++ map ("    " ++) arcLines   -- Add indentation to each arc line
          ++
          [ "}"
          ]

-- Specific converter functions
coulToString :: Coul -> String
coulToString Rouge = "red"
coulToString Noir  = "black"

valToString :: Int -> String
valToString = show

-- Searches for an element in a Binary Search Tree
elementR :: (Ord a) => a -> Arbre c a -> Bool
elementR _ Feuille = False
elementR x (Noeud g _ v d)
    | x == v    = True        -- 1. Found it at the root
    | x < v     = elementR x g  -- 2. It's smaller, so search the left subtree
    | otherwise = elementR x d  -- 3. It's larger, so search the right subtree

-- Inserts a value into the R-B Tree and ensures the root is black
insert :: (Ord a) => a -> Arbre Coul a -> Arbre Coul a
insert x t = noircir (ins x t)

-- Helper to force the root of a tree to be Black
noircir :: Arbre Coul a -> Arbre Coul a
noircir Feuille = Feuille
noircir (Noeud g _ v d) = Noeud g Noir v d

-- Recursive helper for insertion
ins :: (Ord a) => a -> Arbre Coul a -> Arbre Coul a
-- Rule 2: If the tree is empty, insert a new RED node
ins x Feuille = Noeud Feuille Rouge x Feuille
-- Deconstruct the current node
ins x (Noeud g c v d)
    -- Rule 1: If value is already here, return the tree unchanged
    | x == v    = Noeud g c v d
    -- Rule 3 (Left): If value is smaller, insert left and rebalance
    | x < v     = equilibre (Noeud (ins x g) c v d)
    -- Rule 3 (Right): If value is larger, insert right and rebalance
    | otherwise = equilibre (Noeud g c v (ins x d))

-- The rebalancing function
equilibre :: Arbre Coul a -> Arbre Coul a
-- Pattern 1: Left-Left (LL) violation
equilibre (Noeud (Noeud (Noeud a Rouge x b) Rouge y c) Noir z d) =
    Noeud (Noeud a Noir x b) Rouge y (Noeud c Noir z d)
-- Pattern 2: Left-Right (LR) violation
equilibre (Noeud (Noeud a Rouge y (Noeud b Rouge x c)) Noir z d) =
    Noeud (Noeud a Noir y b) Rouge x (Noeud c Noir z d)
-- Pattern 3: Right-Left (RL) violation
equilibre (Noeud a Noir z (Noeud (Noeud b Rouge x c) Rouge y d)) =
    Noeud (Noeud a Noir z b) Rouge x (Noeud c Noir y d)
-- Pattern 4: Right-Right (RR) violation
equilibre (Noeud a Noir z (Noeud b Rouge y (Noeud c Rouge x d))) =
    Noeud (Noeud a Noir z b) Rouge y (Noeud c Noir x d)
-- All other cases: The tree is not in one of the 4 invalid shapes,
-- so return it unchanged.
equilibre t = t

-- Structural Equality (are they identical?)
(===) :: (Eq a, Eq c) => Arbre c a -> Arbre c a -> Bool
(===) = (==)

-- Semantic Equality (do they hold the same data?)
(~==) :: (Eq a) => Arbre c a -> Arbre c a -> Bool
tree1 ~== tree2 =
    (map snd (aplatit tree1)) == (map snd (aplatit tree2))

charToString :: Char -> String
charToString c = [c]

arbresDot :: [Char] -> [String]
arbresDot chars =
    let
        -- 1. Create the list of all intermediate trees.
        -- We start with an empty tree (Feuille) and successively insert
        -- each character from the list.
        -- `flip insert` is used because `scanl` provides the accumulator
        -- (the tree) as the first argument.
        trees :: [ArbreRN Char]
        trees = scanl (flip insert) Feuille chars
        -- 2. Define a function to convert a single tree to its
        --    dot string representation.
        treeToDot :: ArbreRN Char -> String
        treeToDot t = dotise "arbre" coulToString charToString t
    in
        -- 3. Map the `treeToDot` function over our list of trees
        --    to get the final list of strings.
        map treeToDot trees
-- Main function to run the example
main :: IO ()
main = mapM_ ecrit arbres
    where
      -- The list of characters to insert, in "random" order
      chaine = "gcfxieqzrujlmdoywnbakhpvst"
      
      -- The list of dot strings generated from the chain
      arbres = arbresDot chaine

      -- The IO action that writes one string and then pauses
      ecrit :: String -> IO ()
      ecrit a = do
          writeFile "arbre.dot" a
          putStrLn ("Wrote tree state to arbre.dot...")
          threadDelay 1000000 -- 1,000,000 microseconds = 1 second