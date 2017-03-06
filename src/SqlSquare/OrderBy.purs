module SqlSquare.OrderBy where

import Prelude

import Data.Foldable as F
import Data.List as L
import Data.Newtype (class Newtype)
import Data.NonEmpty as NE

import Matryoshka (Algebra)

import SqlSquare.OrderType as OT
import SqlSquare.Utils ((×), type (×))

newtype OrderBy a = OrderBy (NE.NonEmpty L.List (OT.OrderType × a))

derive instance functorOrderBy ∷ Functor OrderBy
derive instance newtypeOrderBy ∷ Newtype (OrderBy a) _
derive instance eqOrderBy ∷ Eq a ⇒ Eq (OrderBy a)
derive instance ordOrderBy ∷ Ord a ⇒ Ord (OrderBy a)

printOrderBy ∷ Algebra OrderBy String
printOrderBy (OrderBy lst) =
  F.intercalate ", " $ lst <#> \(ot × a) → OT.printOrderType ot <> " " <> a
