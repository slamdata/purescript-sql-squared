module SqlSquare.GroupBy where

import Prelude

import Data.Foldable as F
import Data.List as L
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

import Matryoshka (Algebra)

newtype GroupBy a = GroupBy { keys ∷ L.List a, having ∷ Maybe a }
derive instance newtypeGroupBy ∷ Newtype (GroupBy a) _
derive instance functorGroupBy ∷ Functor GroupBy
derive instance eqGroupBy ∷ Eq a ⇒ Eq (GroupBy a)
derive instance ordGroupBy ∷ Ord a ⇒ Ord (GroupBy a)

printGroupBy ∷ Algebra GroupBy String
printGroupBy (GroupBy { keys, having }) =
  F.intercalate ", " keys <> F.foldMap (" having " <> _) having
