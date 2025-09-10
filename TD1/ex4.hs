import qualified Data.Map.Strict as M

ajouter :: Int -> M.Map Int Int -> M.Map Int Int
ajouter k dict = M.insertWith (+) k 1 dict

compte :: [Int] -> [(Int, Int)]
compte l = M.toList (compte' l M.empty)
  where
    compte' :: [Int] -> M.Map Int Int -> M.Map Int Int
    compte' [] dict = dict
    compte' (x:xs) dict = compte' xs (ajouter x dict)

main = do
    print (compte [1,2,1,3,1])