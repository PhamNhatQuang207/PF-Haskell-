import Data.Monoid (Sum(..), Product(..))

expGen :: Monoid a => a -> Int -> a
expGen a n
  | n == 0    = mempty
  | r == 0    = e <> e
  | otherwise = a <> e <> e
  where
    (q, r) = n `quotRem` 2
    e = expGen a q
