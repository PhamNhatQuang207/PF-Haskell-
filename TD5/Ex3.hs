import Ex1
import Ex2
import Control.Applicative (liftA2)
import Control.Monad (mapM)
import Data.Set (Set)
import qualified Data.Set as Set
--3.1
class Finite a where
  allval :: [a]

instance Finite Bool where
  allval = [True, False]

instance Finite (Maybe Bool) where
  allval = [Just True, Just False, Nothing]

instance Finite KBool where
  allval = [KBool (Just True), KBool (Just False), KBool Nothing]

--3.2
instance (Ord a, Finite a) => SemiBool (Set a) where
  (/\) = Set.intersection
  (\/) = Set.union
  neg s = Set.difference (Set.fromList allval) s
  top = Set.fromList allval
  bottom = Set.empty

--3.3
variablesRec :: Formule -> Set String
variablesRec f =
  case f of
    Var s    -> Set.singleton s
    Vrai     -> Set.empty
    Faux     -> Set.empty
    Non f1   -> variablesRec f1
    Et f1 f2 -> Set.union (variablesRec f1) (variablesRec f2)
    Ou f1 f2 -> Set.union (variablesRec f1) (variablesRec f2)

variablesFold :: Formule -> Set String
variablesFold = foldFormule Set.singleton Set.empty Set.empty Set.union Set.union id


--3.4
valuations :: Finite b => [String] -> [[(String, b)]]
valuations vars = mapM (\var -> [(var, v) | v <- allval]) vars


--3.5
data TVerite b = TVerite [Formule] [[b]]

instance Show b => Show (TVerite b) where
    show (TVerite headers rows) =
        let showRow r = unwords $ map (pad 15 . show) r
            headerStr = showRow headers
            line = replicate (length headerStr) '-'
            rowsStr = unlines $ map showRow rows
        in headerStr ++ "\n" ++ line ++ "\n" ++ rowsStr
      where
        pad n s = s ++ replicate (n - length s) ' '

subFormulas :: Formule -> [Formule]
subFormulas f = Set.toList $ go f
  where
    go formula = Set.insert formula $
      case formula of
        Var _ -> Set.empty
        Vrai -> Set.empty
        Faux -> Set.empty
        Non f1 -> go f1
        Et f1 f2 -> Set.union (go f1) (go f2)
        Ou f1 f2 -> Set.union (go f1) (go f2)

truthTable :: (Finite b, SemiBool b, Ord b, Show b) => Formule -> TVerite b
truthTable f = TVerite columns rows
  where
    vars = Set.toList $ variablesFold f
    all_sub_fs = subFormulas f
    columns = map Var vars ++ filter (not . isVar) all_sub_fs
      where isVar (Var _) = True; isVar _ = False
    
    envs = valuations vars
    
    rows = flip map envs $ \env ->
      flip map columns $ \col_formula ->
        case interpSemiBool col_formula env of
          Just val -> val
          Nothing  -> error "Valuation should be complete for truth table generation!"