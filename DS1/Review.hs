monExp a n = go a 1 n
    where
        go a acc 0 = acc
        go a acc n = go a (a*acc) (n-1) 

monExp' a n 
    | r == 1 = a * half * half
    | otherwise = half * half
    where
        (q,r) = quotRem n 2
        half = monExp a q

data ABR a = N a (ABR a) (ABR a) | L

arbres :: ABR (Int,Char)
arbres = N (3,'c') (N (2,'a') L L) L

estElement :: Ord a => a -> ABR a -> Bool
estElement x (N a t1 t2) 
    | x == a = True
    | x < a = estElement x t1
    | otherwise = estElement x t2

assoc :: Ord a => a -> ABR (a,b) -> b
assoc x (N (a,b) t1 t2) 
    | x == a = b
    | x < a = assoc x t1
    | otherwise = assoc x t2

insertBeforeWhen :: (a->Bool) -> a -> [a] -> [a]
insertBeforeWhen p y [] = [y]
insertBeforeWhen p y (x:xs) 
    | p x = y : x : xs
    | otherwise = insertBeforeWhen p y xs

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insertBeforeWhen (>x) x (insertionSort xs)

decoupe :: [a] -> ([a],[a])
decoupe [] = ([],[])
decoupe [x] = ([],[x])
decoupe (x:y:zs) = (x:xs,y:ys)
    where
        (xs,ys) = decoupe zs

fusion :: Ord a => [a] -> [a] -> [a]
fusion l1 [] = l1
fusion [] l2 = l2
fusion (x:xs) (y:ys) 
    | x <= y = x : fusion xs (y:ys)
    | otherwise = y : fusion (x:xs) ys

triFusion :: Ord a => [a] -> [a]
triFusion [] = []
triFusion [x] = [x]
triFusion l = fusion (triFusion xs) (triFusion ys)
    where
        (xs,ys) = decoupe l