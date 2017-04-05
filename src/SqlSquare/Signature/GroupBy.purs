module SqlSquare.Signature.GroupBy where

import Prelude

import Data.Argonaut as J
import Data.Either as E
import Data.Foldable as F
import Data.Traversable as T
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

import Matryoshka (Algebra, CoalgebraM)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

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

encodeJsonGroupBy ∷ Algebra GroupBy J.Json
encodeJsonGroupBy (GroupBy { keys, having }) =
  "tag" J.:= "group by"
  J.~> "keys" J.:= keys
  J.~> "having" J.:= having
  J.~> J.jsonEmptyObject

decodeJsonGroupBy ∷ CoalgebraM (E.Either String) GroupBy J.Json
decodeJsonGroupBy = J.decodeJson >=> \obj → do
  tag ← obj J..? "tag"
  unless (tag == "group by") $ E.Left "This is not group by expression"
  keys ← obj J..? "keys"
  having ← obj J..? "having"
  pure $ GroupBy { keys, having }

arbitraryGroupBy ∷ CoalgebraM Gen.Gen GroupBy Int
arbitraryGroupBy n
  | n == 0 = pure $ GroupBy { having: Nothing, keys: L.Nil }
  | otherwise = do
    len ← Gen.chooseInt 0 $ n - 1
    nothing ← SC.arbitrary
    pure $ GroupBy { having: if nothing then Nothing else Just $ n - 1
                   , keys: map (const $ n - 1) $ L.range 0 len
                   }
