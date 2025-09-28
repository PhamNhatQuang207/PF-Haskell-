f a b
    | even a = q*(a+b)
    | otherwise = f q b
    where q = a `quot` 2

g :: (Ord a, Fractional a) => a -> a -> a -> a
g a b c
    | c > 0 =a*b/c
    | b > 0 = a*c/b
    | otherwise = a*b*c

h :: Ord a => a -> a -> a -> a 
h a b c = if a>b && b>c 
          then b
          else 
            if a>c && b>c
            then c
            else a

k [] = 0
k [_] = 0
k (a:l@(b:_)) = if a>b 
                then 1 + k l
                else k l