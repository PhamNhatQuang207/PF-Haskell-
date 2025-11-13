import Prelude hiding (Functor, Applicative, Monad, return, (>>), (>>=), pure, (<*>), fmap)
import Control.Applicative (Alternative(..))

infixl 4 <*>
infixl 1 >>, >>=
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
  return :: a -> m a          
  return = pure   
  (>>=) :: m a -> (a -> m b) -> m b 

  (>>) :: m a -> m b -> m b
  ma >> mb = ma >>= \_ -> mb

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  (Just f) <*> (Just x) = Just (f x)

instance Monad Maybe where
  return = pure
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x


ex1 :: Num a => Maybe a
ex1 = (+1) <$> (Nothing :: Maybe a)

ex2 :: Num a => Maybe [a]
ex2 = (1:) <$> Just ([2,3,4])

ex3 :: (Num a, Ord a) => Maybe a
ex3 = (\n -> if n > 0 then n*2 else 0) <$> (if 4*10 < 20 then Nothing else Just (-1))

ex4 :: Num a => Maybe (a -> a)
ex4 = pure (\k -> 2*k+1)

ex5 :: Integral a => Maybe a
ex5 = pure (\k -> if even k then k `quot` 2 else k * 3) <*> Just (17 * 6)

ex6 :: Num a => Maybe a
ex6 = (+) <$> Just 4 <*> (Nothing :: Maybe a)

ex7 :: Num a => Maybe (a->a)
ex7 = (\a b c -> a * b + c) <$> ((+) <$> Just 5 <*> Just 6) <*> Just 7

ex8 :: (Num a, Ord a) => Maybe a
ex8 = Just 5 >>= \k -> if k < 10 then return 4 else return 20

ex9 :: (Num a, Ord a) => Maybe a
ex9 = Nothing >>= \k -> if k < 10 then return 4 else return 20

ex10 :: (Num a, Ord a) => Maybe a -> Maybe Bool
ex10 m = do
    a <- m
    if a == 0 then return True else empty

findSafe :: (a -> Bool) -> [a] -> Maybe a
findSafe _ [] = Nothing
findSafe p (a:l) = case findSafe p l of
                   Just r  -> Just r
                   Nothing -> if p a then Just a else Nothing

findSafe' :: (a -> Bool) -> [a] -> Maybe a
findSafe' _ [] = empty  
findSafe' p (a:l) =
    findSafe' p l <|> (if p a then pure a else empty)
