import Prelude hiding (Functor, Applicative, Monad, return, (>>), (>>=), pure, (<*>), fmap)
import Control.Applicative (Alternative(..))

infixl 4  <*>
infixl 1 >>=, >>
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

instance Functor ((->) r) where
  fmap f g = \r -> f (g r)

instance Applicative ((->) r) where
  pure a = \r -> a
  f <*> g = \r -> (f r) (g r)
instance Monad ((->) r) where
    f >>= h = \r -> h (f r) r

ex1 :: Bool -> Int
ex1 = (*2) <$> \r -> if r then 5 else 10

ex2 :: [Int] -> Int
ex2 = (\n -> if even n then n `quot` 2 else n * 3) <$> \l -> if l == [] then 0 else 15

ex3 :: Bool -> Integer
ex3 = pure (\k -> 2*k + 1) <*> (\b -> if b then 5 else 4)

ex4 :: [Int] -> Int
ex4 = (\a b c -> if b then a else c) <$> sum <*> all even <*> product


ask :: (->) r r
ask = id

local :: (r -> r) -> ((->) r a) -> ((->) r a)
local f g r= g (f r)
-- Le type Somme a, permet de représenter des arbres de calculs de sommes de
-- valeurs de type a.
-- Ils contiennent des variables dont la valeur sera fixée
-- par une environnement d'évaluation.
data Somme a =
    Var String
    -- Var x représente une variable dont le nom est x
  | Val a
    -- Val a est une valeur de type a
  | S (Somme a) (Somme a) -- S s1 s2 représente la somme
    -- des valeurs de s1 et de s2.
  | Lier String (Somme a) (Somme a) -- Lier x s1 s2, représente le calcul suivant:
    -- let x = s1 in s2.
    -- Ainsi le nom x est lié à la valeur
    -- calculée par s1 pour évaluer s2.

-- Pour évaluer une valeur de type, Somme a, nous avons besoin d'un
-- environnement qui à une chaine de caractères associe une valeur,
-- fonction de type String -> a. Ainsi une valeur de type Somme a s'évalue en
-- une fonction de type (String -> a) -> a. Or l'environnement est partagé par
-- toutes les sous-expressions. Nous pouvons ainsi utiliser la monade Reader
-- pour définir l'évaluation d'une valeur de type Somme a.
evalS :: Num a => Somme a -> (String -> a) -> a

-- Pour évaluer une variable, on récupère l'environnement et on renvoie la
-- valeur qu'il associe au nom de la variable.
evalS (Var x) = ask >>= \env -> return (env x)

-- Une valeur est simplement renvoyée
evalS (Val a) = return a

-- Une somme est calculée simplement en appliquant l'addition aux résultats de
-- l'évaluation récursive des termes à sommer.
evalS (S s1 s2) = (+) <$> evalS s1 <*> evalS s2

-- Pour lier la variable x à la valeur calculée par s1, on utilise local qui
-- permet de modifier l'environnement. Puis on évalue s2 dans ce nouvel
-- environnenent.
evalS (Lier x s1 s2) = local (\env s -> if s==x then evalS s1 env else env s)
                             (evalS s2)

-- Voici un exemple de terme de type Somme Int
t :: Somme Int
t = S (Val 1) (S (Lier "x" (Val 3) (S (Var "x") (Var "x"))) (Var "x"))

-- Voici un exemple d'environnement. Maintenant que vous avez fini de lire
-- inscrivez sur votre copie : "j'ai bien lu le code"
env1 :: String -> Int
env1 "x" = 1
env1 _   = 0