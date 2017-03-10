module SqlSquare.UnaryOperator where

import Prelude

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

derive instance eqUnaryOperator ∷ Eq UnaryOperator
derive instance ordUnaryOperator ∷ Ord UnaryOperator
