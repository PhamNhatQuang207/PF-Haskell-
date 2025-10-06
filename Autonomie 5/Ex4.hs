import Control.Applicative
mystere' :: [[a]] -> [[a]]
mystere' = foldr f (repeat [])
  where
    f l tr = getZipList $ (:) <$> ZipList l <*> ZipList tr