module SqlSquare.GroupBy where

import Prelude

import Data.Foldable as F
import Data.Traversable as T
import Data.List as L
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

import Matryoshka (Algebra)

newtype GroupBy a = GroupBy { keys ∷ L.List a, having ∷ Maybe a }
derive instance newtypeGroupBy ∷ Newtype (GroupBy a) _
derive instance functorGroupBy ∷ Functor GroupBy
derive instance eqGroupBy ∷ Eq a ⇒ Eq (GroupBy a)
derive instance ordGroupBy ∷ Ord a ⇒ Ord (GroupBy a)

instance foldableGroupBy ∷ F.Foldable GroupBy where
  foldMap f (GroupBy { keys, having }) = F.foldMap f keys <> F.foldMap f having
  foldl f a (GroupBy { keys, having }) = F.foldl f (F.foldl f a keys) having
  foldr f a (GroupBy { keys, having }) = F.foldr f (F.foldr f a having) keys

instance traversableGroupBy ∷ T.Traversable GroupBy where
  traverse f (GroupBy { keys, having }) =
    map GroupBy $ {keys: _, having: _} <$> T.traverse f keys <*> T.traverse f having
  sequence = T.sequenceDefault

printGroupBy ∷ Algebra GroupBy String
printGroupBy (GroupBy { keys, having }) =
  F.intercalate ", " keys <> F.foldMap (" HAVING " <> _) having
