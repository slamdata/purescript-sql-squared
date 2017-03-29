module SqlSquare.UnaryOperator where

import Prelude

import Data.Argonaut as J
import Data.Either (Either(..))
import Data.List ((:))
import Data.List as L

import Test.StrongCheck.Arbitrary as A
import Test.StrongCheck.Gen as Gen

data UnaryOperator
  = Not
  | Exists
  | Positive
  | Negative
  | Distinct
  | FlattenMapKeys
  | FlattenMapValues
  | ShiftMapKeys
  | ShiftMapValues
  | FlattenArrayIndices
  | FlattenArrayValues
  | ShiftArrayIndices
  | ShiftArrayValues
  | UnshiftArray

unopToString ∷ UnaryOperator → String
unopToString = case _ of
  Not → "not"
  Exists → "exists"
  Positive → "positive"
  Negative → "negative"
  Distinct → "distinct"
  FlattenMapKeys → "flatten map keys"
  FlattenMapValues → "flatten map values"
  ShiftMapKeys → "shift map keys"
  ShiftMapValues → "shift map values"
  FlattenArrayIndices → "flatten array indices"
  FlattenArrayValues → "flatten array values"
  ShiftArrayIndices → "shift array indices"
  ShiftArrayValues → "shift array values"
  UnshiftArray → "unshift array"

unopFromString ∷ String → Either String UnaryOperator
unopFromString = case _ of
  "not" → Right Not
  "exists" → Right Exists
  "positive" → Right Positive
  "negative" → Right Negative
  "distinct" → Right Distinct
  "flatten map keys" → Right FlattenMapKeys
  "flatten map values" → Right FlattenMapValues
  "shift map keys" → Right ShiftMapKeys
  "shift map values" → Right ShiftMapValues
  "flatten array indices" → Right FlattenArrayIndices
  "flatten array values" → Right FlattenArrayValues
  "shift array indices" → Right ShiftArrayIndices
  "shift array vlaues" → Right ShiftArrayValues
  "unshift array" → Right UnshiftArray
  _ → Left "This is not an unary operator"

derive instance eqUnaryOperator ∷ Eq UnaryOperator
derive instance ordUnaryOperator ∷ Ord UnaryOperator

instance encodeJsonUnaryOperator ∷ J.EncodeJson UnaryOperator where
  encodeJson op =
    "tag" J.:= "unary operator"
    J.~> "value" J.:= unopToString op
    J.~> J.jsonEmptyObject

instance decodeJsonUnaryOperator ∷ J.DecodeJson UnaryOperator where
  decodeJson = J.decodeJson >=> \obj → do
    tag ← obj J..? "tag"
    unless (tag == "unary operator")
      $ Left "This is not a unary operator"
    (obj J..? "value") >>= unopFromString

instance arbitaryUnaryOperator ∷ A.Arbitrary UnaryOperator where
  arbitrary =
    Gen.elements Not
      $ Exists : Positive : Negative : Distinct : FlattenMapKeys
      : FlattenMapValues : ShiftMapKeys : ShiftMapValues
      : FlattenArrayIndices : FlattenArrayValues
      : ShiftArrayIndices : ShiftArrayValues
      : UnshiftArray : L.Nil
