module SqlSquare.BinaryOperator where

import Prelude

data BinaryOperator
  = IfUndefined
  | Range
  | Or
  | And
  | Eq
  | Neq
  | Ge
  | Gt
  | Le
  | Lt
  | Concat
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Pow
  | In
  | FieldDeref
  | IndexDeref
  | Limit
  | Offset
  | Sample
  | Union
  | UnionAll
  | Intersect
  | IntersectAll
  | Except
  | UnshiftMap

derive instance eqBinaryOperator ∷ Eq BinaryOperator
derive instance ordBinaryOperator ∷ Ord BinaryOperator
