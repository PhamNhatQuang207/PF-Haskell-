from0 :: Int -> [Int]
from0 n
  | n < 0     = error "n must be >= 0"
  | otherwise = go n []
  where
    go 0 l = 0 : l
    go k l = go (k-1) (k : l)