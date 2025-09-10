decoupe :: [Int] -> ([Int], [Int])
decoupe [] = ([], [])
decoupe [x] = ([], [x])
decoupe (x:y:zs) = (x:xs, y:ys)
  where
    (xs, ys) = decoupe zs

fusion :: [Int] -> [Int] -> [Int]
fusion xs [] = xs
fusion [] ys = ys
fusion (x:xs) (y:ys)
  | x <= y    = x : fusion xs (y:ys)
  | otherwise = y : fusion (x:xs) ys


triFusion :: [Int] -> [Int]
triFusion [] = []
triFusion [x] = [x]
triFusion xs = fusion (triFusion firstHalf) (triFusion secondHalf)
  where
    (firstHalf, secondHalf) = decoupe xs

compteTri :: [Int] -> [(Int, Int)]
compteTri [] = []
compteTri (x:xs) = cptAux x 1 xs
  where
    cptAux :: Int -> Int -> [Int] -> [(Int, Int)]
    cptAux n cpt [] = [(n, cpt)]
    cptAux n cpt (y:ys)
      | n == y    = cptAux n (cpt + 1) ys
      | otherwise = (n, cpt) : cptAux y 1 ys

main = do
    let unsortedList = [1, 2, 1, 3, 1]
    let sortedList = triFusion unsortedList
    print (compteTri sortedList)