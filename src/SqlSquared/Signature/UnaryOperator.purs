module SqlSquared.Signature.UnaryOperator where

import Prelude

import Data.Argonaut as J
import Data.Either (Either(..))
import Data.NonEmpty ((:|))
import Test.QuickCheck.Arbitrary as A
import Test.QuickCheck.Gen as Gen

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
  "shift array values" → Right ShiftArrayValues
  "unshift array" → Right UnshiftArray
  a → Left $ "This is not an unary operator "  <> a

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
      $ Left $ "This is not a unary operator " <> tag
    (obj J..? "value") >>= unopFromString

instance arbitaryUnaryOperator ∷ A.Arbitrary UnaryOperator where
  arbitrary =
    Gen.elements $ Not :|
      [ Exists, Positive, Negative, Distinct, FlattenMapKeys
      , FlattenMapValues, ShiftMapKeys, ShiftMapValues
      , FlattenArrayIndices, FlattenArrayValues
      , ShiftArrayIndices, ShiftArrayValues
      , UnshiftArray
      ]

printUnaryOperator ∷ String → UnaryOperator → String
printUnaryOperator expr = case _ of
  Not → "NOT " <> expr
  Exists → "EXISTS " <> expr
  Positive → "+" <> expr
  Negative → "-" <> expr
  Distinct → "DISTINCT " <> expr
  FlattenMapKeys → expr <> "{*:}"
  FlattenMapValues → expr <> "{*}"
  ShiftMapKeys → expr <> "{_:}"
  ShiftMapValues → expr <> "{_}"
  FlattenArrayIndices → expr <> "[*:]"
  FlattenArrayValues → expr <> "[*]"
  ShiftArrayIndices → expr <> "[_:]"
  ShiftArrayValues → expr <> "[_]"
  UnshiftArray → "[" <> expr <> "...]"
