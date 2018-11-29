module Pred where
import Debug.Trace (trace)

import Util (showList, showMap, onPair)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)

import Data.List ((\\))
import Operation (Sym (S), Instruction, Op)

data Atom
  = Value Sym -- a particular symbol
  | Variable Sym -- a variable that can match any symbol (in its context)
  | Predicate Pred
  deriving (Eq, Ord)

instance Show Atom where
  show (Value s) = show s
  show (Variable s) = "{"++show s++"}"
  show (Predicate atoms) = show atoms

newtype Pred = Pred (Map Sym Atom) deriving (Eq, Ord)

instance Show Pred where
  show (Pred atoms) = "("++Util.showMap atoms++")"

type State = Set Pred
type Assignment = Map Sym Atom

data ResolutionFailure
  = VariableAssignmentContradiction
    { key :: Sym
    , value :: Atom
    , in_ :: Assignment
    }
  | ConcreteMismatch
    { key :: Sym
    , value :: Atom
    , in_ :: Assignment
    }
  | PredicatesOfDifferentShapes
    { requirement :: Pred
    , possible_solution :: Pred
    }
    deriving (Eq, Ord, Show)

data System a
  = Partial a
  | Error ResolutionFailure
  deriving (Eq, Ord, Show)

instance Functor System where
  fmap f (Partial a) = Partial $ f a
  fmap _ (Error e) = (Error e)

instance Applicative System where
  pure = Partial
  (Partial fs) <*> (Partial xs) = Partial $ fs xs
  (Error fs) <*> _ = Error fs
  _ <*> (Error xs) = Error xs

instance Monad System where
  (Partial xs) >>= f = f xs
  (Error xs) >>= _ = Error xs

type Resolution = System Assignment

instance Semigroup Resolution where
  xs <> ys = xs >>= M.foldrWithKey restrictOne ys

instance Monoid Resolution where
  mempty :: Resolution
  mempty = pure M.empty


ignoreErrors :: [Resolution] -> [Assignment]
ignoreErrors xs = foldr next' [] xs
  where
    next' (Partial ass) xs' = ass:xs'
    next' (Error _) xs' = xs'

val :: String -> Atom
val = Value . S

var :: String -> Atom
var = Variable . S

toMap :: Pred -> Map Sym Atom
toMap (Pred atoms) = atoms

toPred :: [(String, Atom)] -> Pred
toPred xs = Pred $ M.fromList $ map (onPair S id) xs

exists :: Atom -> Pred
exists v = toPred [("exists", v)]

emptyState :: State
emptyState = S.empty

-- TODO(jopra): Should check that each value is defined (not just used)
solutions :: State -> State -> [Assignment]
solutions known preds
   = ignoreErrors $ resolution known (S.toList preds) $ mempty

-- Finds assignments (that are specialisations of the input assignment) for which the Preds are resolvable.
resolution :: State -> [Pred] -> Resolution -> [Resolution]
resolution known ps ass = foldr (concatMap.(partialResolution known)) [ass] ps

partialResolution :: State -> Pred -> Resolution -> [Resolution]
partialResolution known pred ass
  = do
  poss <- S.toList known
  return $ ass <> assignmentFromPred pred poss

restrictOne :: Sym -> Atom -> Resolution -> Resolution
restrictOne k v (Partial xs)
  = case M.lookup k xs of
      Nothing -> return $ M.insert k v xs
      Just v' -> if v == v'
                    then return xs
                    else Error $ VariableAssignmentContradiction {key = k, value = v, in_ = xs}
restrictOne _ _ err = err

restrictAtoms :: Atom -> Atom -> Resolution -> Resolution
restrictAtoms _ _ err@(Error _) = err
restrictAtoms (Value k) (Value v) ass
  = if k == v
       then ass
       else (\ass' -> Error $ ConcreteMismatch { key = k, value = Value v, in_ = ass' }) =<< ass
restrictAtoms (Predicate vs) (Predicate xs) ass = (assignmentFromPred vs xs) <> ass
restrictAtoms (Variable k) (Value v) ass = restrictOne k (Value v) ass
restrictAtoms (Variable (S k)) (Predicate (Pred xs)) ass = do
  M.foldrWithKey (\(S var)->restrictOne (S$k<>"."<>var)) ass xs
restrictAtoms k v ass = error $ "Unimplemented restrictAtoms for: k:"++show k ++" v:"++ show v

assignmentFromPred :: Pred -> Pred -> Resolution
assignmentFromPred pred poss
  | M.keysSet (toMap pred) /= M.keysSet (toMap poss) = Error $
    PredicatesOfDifferentShapes { requirement = pred, possible_solution = poss}
  | otherwise = ass''
  where
    ass'' = foldr (uncurry restrictAtoms) mempty ass'
    ass' = M.intersectionWithKey (\k pr po -> (pr, po)) (toMap pred) (toMap poss)
