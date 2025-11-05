duplique :: [a] -> [[a]]
duplique [] = []
duplique (x:xs) = [x,x] : duplique xs

duplique'  = map (\x -> [x,x]) 

duplique'' l = [[x,x]| x <- l]

coupleDistincts :: Ord a => [a] -> [(a,a)]
coupleDistincts [] = []
coupleDistincts xs = [(x,y) | x <- xs, y <- xs, x/= y]

coupleDistincts' xs = concatMap (\x -> map(\y -> (x,y)) (filter (/=x) xs)) xs