
import Data.List
h1 x y l = map (+(x*y)) l

h1' = ((map .) . ((+) .) . (*))

h2 l = map (\a -> a : l) l

h2' l = map (:l) l

h3 l1 l2 l3 = zipWith (\a b -> a*sum l1 +b) l2 l3

h3' l1 = zipWith ((+) . (* sum l1))

h4 :: Num a => [[a]] -> a
h4 = product . map sum . transpose

h4' matrix = product (map sum (transpose matrix))


h5 n = map (+1) . filter even . take n

h5' n l = map (+1) (filter even (take n l))