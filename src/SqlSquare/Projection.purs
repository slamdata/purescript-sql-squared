module SqlSquare.Projection where

import Prelude

import Data.Foldable as F
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

import Matryoshka (Algebra)

newtype Projection a = Projection { expr ∷ a, alias ∷ Maybe String }

derive instance functorProjection ∷ Functor Projection
derive instance newtypeProjection ∷ Newtype (Projection a) _
derive instance eqProjection ∷ Eq a ⇒ Eq (Projection a)
derive instance ordProjection ∷ Ord a ⇒ Ord (Projection a)

printProjection ∷ Algebra Projection String
printProjection (Projection { expr, alias }) = expr <> F.foldMap (" as " <> _) alias
