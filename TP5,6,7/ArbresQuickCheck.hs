module ArbresQuickCheck where

import Test.QuickCheck
import Arbres

-- Property: height of left comb equals the number of nodes (length of list)
prop_hauteurPeigne :: [(c,a)] -> Bool
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)

-- Types used for a specific property
type C = ()
type A = Int

-- Arbitrary instance for generating random trees
instance (Arbitrary c, Arbitrary a) => Arbitrary (Arbre c a) where
  arbitrary = sized createTree
    where
      createTree n
        | n <= 0    = return Feuille
        | otherwise = frequency
            [ (1, return Feuille)
            , (4, Noeud <$> resize (n `div` 2) arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> resize (n `div` 2) arbitrary)
            ]

-- Property: a perfect tree that is also a left comb must be of height <= 1
prop_perfectCombIsTrivial :: Arbre C A -> Property
prop_perfectCombIsTrivial tree =
    (estParfait tree && isLeftComb tree) ==> (hauteur tree <= 1)

-- Simple test runner
main :: IO ()
main = do
  quickCheck prop_hauteurPeigne
  quickCheck prop_perfectCombIsTrivial
