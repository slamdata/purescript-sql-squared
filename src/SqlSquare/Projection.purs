module SqlSquare.Projection where

import Prelude

import Data.Foldable as F
import Data.Traversable as T
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

import Matryoshka (Algebra)

import SqlSquare.Utils ((∘))

newtype Projection a = Projection { expr ∷ a, alias ∷ Maybe String }

derive instance functorProjection ∷ Functor Projection
derive instance newtypeProjection ∷ Newtype (Projection a) _
derive instance eqProjection ∷ Eq a ⇒ Eq (Projection a)
derive instance ordProjection ∷ Ord a ⇒ Ord (Projection a)
instance foldableProjection ∷ F.Foldable Projection where
  foldMap f (Projection { expr }) = f expr
  foldl f a (Projection { expr }) = f a expr
  foldr f a (Projection { expr }) = f expr a
instance traversableProjection ∷ T.Traversable Projection where
  traverse f (Projection { expr, alias }) =
    map (Projection ∘ { expr: _, alias}) $ f expr
  sequence = T.sequenceDefault

printProjection ∷ Algebra Projection String
printProjection (Projection { expr, alias }) = expr <> F.foldMap (" AS " <> _) alias
