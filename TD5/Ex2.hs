module Ex2 where
import Ex1
import Control.Applicative (liftA2)

interpBool_v1 :: Formule -> [(String, Bool)] -> Maybe Bool
interpBool_v1 f env =
  case f of
    Vrai      -> Just True
    Faux      -> Just False
    Var s     -> lookup s env  
    Non f1    -> case interpBool_v1 f1 env of
                   Just b  -> Just (not b)
                   Nothing -> Nothing
    Et f1 f2  -> case interpBool_v1 f1 env of
                   Nothing -> Nothing
                   Just b1 -> case interpBool_v1 f2 env of
                                Nothing -> Nothing
                                Just b2 -> Just (b1 && b2)
    Ou f1 f2  -> case interpBool_v1 f1 env of
                   Nothing -> Nothing
                   Just b1 -> case interpBool_v1 f2 env of
                                Nothing -> Nothing
                                Just b2 -> Just (b1 || b2)

interpBool_v2 :: Formule -> [(String, Bool)] -> Maybe Bool
interpBool_v2 f env =
  case f of
    Vrai      -> Just True
    Faux      -> Just False
    Var s     -> lookup s env
    Non f1    -> not <$> interpBool_v2 f1 env
    Et f1 f2  -> liftA2 (&&) (interpBool_v2 f1 env) (interpBool_v2 f2 env)
    Ou f1 f2  -> liftA2 (||) (interpBool_v2 f1 env) (interpBool_v2 f2 env)

interpBool_v3 :: Formule -> [(String, Bool)] -> Maybe Bool
interpBool_v3 f env = foldFormule fVar fVrai fFaux fEt fOu fNon f
  where
    fVar s   = lookup s env
    fVrai    = Just True
    fFaux    = Just False
    fEt      = liftA2 (&&)
    fOu      = liftA2 (||)
    fNon     = fmap not

class SemiBool a where
  (/\)   :: a -> a -> a -- AND
  (\/)   :: a -> a -> a -- OR
  neg    :: a -> a      -- NOT
  top    :: a           -- True
  bottom :: a           -- False


--2.3
instance SemiBool Bool where
  (/\)   = (&&)
  (\/)   = (||)
  neg    = not
  top    = True
  bottom = False


--2.4
interpSemiBool :: SemiBool a => Formule -> [(String, a)] -> Maybe a
interpSemiBool f env = foldFormule fVar fVrai fFaux fEt fOu fNon f
  where
    fVar s   = lookup s env
    fVrai    = Just top
    fFaux    = Just bottom
    fEt      = liftA2 (/\)
    fOu      = liftA2 (\/)
    fNon     = fmap neg


--2.5
instance SemiBool (Maybe Bool) where
  top    = Just True
  bottom = Just False
  neg    = fmap not

  mx /\ my = do
    x <- mx
    if x then my else pure False

  mx \/ my = do
    x <- mx
    if x then pure True else my

--2.6
newtype KBool = KBool { getKBool :: Maybe Bool } deriving (Show, Eq)

instance SemiBool KBool where
  top    = KBool (Just True)
  bottom = KBool (Just False)
  neg (KBool mx) = KBool (fmap not mx)
  (KBool mx) /\ (KBool my) = KBool $
    case (mx, my) of
      (Just False, _) -> Just False
      (_, Just False) -> Just False
      (Just True, Just True) -> Just True
      _ -> Nothing 
  (KBool mx) \/ (KBool my) = KBool $
    case (mx, my) of
      (Just True, _) -> Just True
      (_, Just True) -> Just True
      (Just False, Just False) -> Just False
      _ -> Nothing

--2.7
instance SemiBool b => SemiBool (a -> b) where
  f /\ g = \x -> (f x) /\ (g x)
  f \/ g = \x -> (f x) \/ (g x)

  neg f  = \x -> neg (f x)

  top    = \_ -> top
  bottom = \_ -> bottom