module SqlSquare.Signature.OrderBy where

import Prelude

import Data.Argonaut as J
import Data.Either as E
import Data.Foldable as F
import Data.Traversable as T
import Data.List as L
import Data.Newtype (class Newtype)
import Data.NonEmpty as NE

import Matryoshka (Algebra, CoalgebraM)

import SqlSquare.Signature.OrderType as OT

import SqlSquare.Utils ((×), type (×))

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

newtype OrderBy a = OrderBy (NE.NonEmpty L.List (OT.OrderType × a))

derive instance functorOrderBy ∷ Functor OrderBy
derive instance newtypeOrderBy ∷ Newtype (OrderBy a) _
derive instance eqOrderBy ∷ Eq a ⇒ Eq (OrderBy a)
derive instance ordOrderBy ∷ Ord a ⇒ Ord (OrderBy a)
instance foldableOrderBy ∷ F.Foldable OrderBy where
  foldMap f (OrderBy xs) = F.foldMap (F.foldMap f) xs
  foldl f a (OrderBy xs) = F.foldl (F.foldl f) a xs
  foldr f a (OrderBy xs) = F.foldr (flip (F.foldr f)) a xs
instance traversableOrderBy ∷ T.Traversable OrderBy where
  traverse f (OrderBy xs) = map OrderBy $ T.traverse (T.traverse f) xs
  sequence = T.sequenceDefault

printOrderBy ∷ Algebra OrderBy String
printOrderBy (OrderBy lst) =
  F.intercalate ", " $ lst <#> \(ot × a) → a <> " " <> OT.printOrderType ot

encodeJsonOrderBy ∷ Algebra OrderBy J.Json
encodeJsonOrderBy (OrderBy lst) =
  "tag" J.:= "order by"
  J.~> "value" J.:= (L.singleton (NE.head lst) <> NE.tail lst)
  J.~> J.jsonEmptyObject

decodeJsonOrderBy ∷ CoalgebraM (E.Either String) OrderBy J.Json
decodeJsonOrderBy = J.decodeJson >=> \obj → do
  tag ← obj J..? "tag"
  unless (tag == "order by") $ E.Left "This is not order by expression"
  lst ← obj J..? "value"
  case lst of
    L.Nil → E.Left "This is not order by expression"
    L.Cons hd tail → pure $ OrderBy $ hd NE.:| tail

arbitraryOrderBy ∷ CoalgebraM Gen.Gen OrderBy Int
arbitraryOrderBy n
  | n < 2 = do
    ot ← SC.arbitrary
    pure $ OrderBy $ (ot × n - 1) NE.:| L.Nil
  | otherwise = do
    len ← Gen.chooseInt 0 $ n - 1
    let
      foldFn acc _ = do
        ot ← SC.arbitrary
        pure $ (ot × n - 1) L.: acc

    lst ← L.foldM foldFn L.Nil $ L.range 0 len
    ot ← SC.arbitrary
    pure $ OrderBy $ (ot × n - 1) NE.:| lst
