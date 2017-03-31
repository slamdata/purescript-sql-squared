module SqlSquare.Projection where

import Prelude

import Data.Argonaut as J
import Data.Either as E
import Data.Foldable as F
import Data.Traversable as T
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

import Matryoshka (Algebra, CoalgebraM)

import SqlSquare.Utils ((∘))

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

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

encodeJsonProjection ∷ Algebra Projection J.Json
encodeJsonProjection (Projection {expr, alias}) =
  "tag" J.:= "projection"
  J.~> "expr" J.:= expr
  J.~> "alias" J.:= alias
  J.~> J.jsonEmptyObject

decodeJsonProjection ∷ CoalgebraM (E.Either String) Projection J.Json
decodeJsonProjection = J.decodeJson >=> \obj → do
  tag ← obj J..? "tag"
  unless (tag == "projection") $ E.Left "This is not a projection"
  expr ← obj J..? "expr"
  alias ← obj J..? "alias"
  pure $ Projection { expr, alias }

arbitraryProjection ∷ CoalgebraM Gen.Gen Projection Int
arbitraryProjection n = do
  alias ← SC.arbitrary
  pure $ Projection { expr: n - 1, alias }
