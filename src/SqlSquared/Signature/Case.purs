module SqlSquared.Signature.Case where

import Prelude

import Control.Monad.Gen as Gen
import Data.Foldable as F
import Data.Newtype (class Newtype)
import Data.Traversable as T
import Matryoshka (Algebra, CoalgebraM)

newtype Case a = Case { cond ∷ a, expr ∷ a }

derive instance functorCase ∷ Functor Case
derive instance newtypeCase ∷ Newtype (Case a) _
derive instance eqCase ∷ Eq a ⇒ Eq (Case a)
derive instance ordCase ∷ Ord a ⇒ Ord (Case a)

instance foldableCase ∷ F.Foldable Case where
  foldMap f (Case { expr }) = f expr
  foldl f a (Case { cond, expr }) = f (f a cond) expr
  foldr f a (Case { cond, expr }) = f cond $ f expr a

instance traversableCase ∷ T.Traversable Case where
  traverse f (Case { cond, expr }) = map Case $ { cond: _, expr: _ } <$> f cond <*> f expr
  sequence = T.sequenceDefault

printCase ∷ Algebra Case String
printCase (Case { cond, expr }) = "WHEN " <> cond <> " THEN " <> expr

genCase ∷ ∀ m. Gen.MonadGen m ⇒ CoalgebraM m Case Int
genCase n
  | n < 2 = pure $ Case { cond: 0, expr: 0 }
  | otherwise = pure $ Case { cond: n - 1, expr: n - 1 }
