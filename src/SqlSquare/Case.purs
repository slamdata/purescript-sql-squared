module SqlSquare.Case where

import Prelude

import Data.Newtype (class Newtype)
import Data.Foldable as F
import Data.Traversable as T

import Matryoshka (Algebra)

newtype Case a = Case { cond ∷ a, expr ∷ a }

derive instance functorCase ∷ Functor Case
derive instance newtypeCase ∷ Newtype (Case a) _
derive instance eqCase ∷ Eq a ⇒ Eq (Case a)
derive instance ordCase ∷ Ord a ⇒ Ord (Case a)

instance foldableCase ∷ F.Foldable Case where
  foldMap f (Case { cond, expr }) = f expr
  foldl f a (Case { cond, expr }) = f (f a cond) expr
  foldr f a (Case { cond, expr }) = f cond $ f expr a
instance traversableCase ∷ T.Traversable Case where
  traverse f (Case { cond, expr }) = map Case $ { cond: _, expr: _ } <$> f cond <*> f expr
  sequence = T.sequenceDefault

printCase ∷ Algebra Case String
printCase (Case { cond, expr }) = " when " <> cond <> " then " <> expr
