module SqlSquared.Signature.BinaryOperator where

import Prelude

import Control.Monad.Gen as Gen
import Data.Either (Either(..))
import Data.NonEmpty ((:|))

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

binopToString ∷ BinaryOperator → String
binopToString = case _ of
  IfUndefined → "if undefined"
  Range → "range"
  Or → "or"
  And → "and"
  Eq → "eq"
  Neq → "neq"
  Ge → "ge"
  Gt → "gt"
  Le → "le"
  Lt → "lt"
  Concat → "concat"
  Plus → "plus"
  Minus → "minus"
  Mult → "mult"
  Div → "div"
  Mod → "mod"
  Pow → "pow"
  In → "in"
  FieldDeref → "field deref"
  IndexDeref → "index deref"
  Limit → "limit"
  Offset → "offset"
  Sample → "sample"
  Union → "union"
  UnionAll → "union all"
  Intersect → "intersect"
  IntersectAll → "intersect all"
  Except → "except"
  UnshiftMap → "unshift map"

binopFromString ∷ String → Either String BinaryOperator
binopFromString = case _ of
  "if undefined" → Right IfUndefined
  "range" → Right Range
  "or" → Right Or
  "and" → Right And
  "eq" → Right Eq
  "neq"→ Right Neq
  "ge" → Right Ge
  "gt" → Right Gt
  "le" → Right Le
  "lt" → Right Lt
  "concat" → Right Concat
  "plus" → Right Plus
  "minus" → Right Minus
  "mult" → Right Mult
  "div" → Right Div
  "mod" → Right Mod
  "pow" → Right Pow
  "in" → Right In
  "field deref" → Right FieldDeref
  "index deref" → Right IndexDeref
  "limit" → Right Limit
  "offset" → Right Offset
  "sample" → Right Sample
  "union" → Right Union
  "union all" → Right UnionAll
  "intersect" → Right Intersect
  "intersect all" → Right IntersectAll
  "except" → Right Except
  "unshift map" → Right UnshiftMap
  _ → Left "This is not a binary operator"


derive instance eqBinaryOperator ∷ Eq BinaryOperator
derive instance ordBinaryOperator ∷ Ord BinaryOperator

genBinaryOperator ∷ ∀ m. Gen.MonadGen m ⇒ m BinaryOperator
genBinaryOperator =
  Gen.elements $ IfUndefined :|
    [ Range, Or, And, Eq, Neq, Ge, Gt, Le, Lt
    , Concat, Plus, Minus, Mult, Div, Mod, Pow
    , In, FieldDeref, IndexDeref, Limit, Offset
    , Sample, Union, UnionAll, Intersect
    , IntersectAll, Except, UnshiftMap
    ]

printBinaryOperator ∷ String → String → BinaryOperator → String
printBinaryOperator lhs rhs = case _ of
  IfUndefined → lhs <> " ?? " <> rhs
  Range → lhs <> " .. " <> rhs
  Or → lhs <> " OR " <> rhs
  And → lhs <> " AND " <> rhs
  Eq → lhs <> " = " <> rhs
  Neq → lhs <> " <> " <> rhs
  Ge → lhs <> " >= " <> rhs
  Gt → lhs <> " > " <> rhs
  Le → lhs <> " <= " <> rhs
  Lt → lhs <> " < " <> rhs
  Concat → lhs <> " || " <> rhs
  Plus → lhs <> " + " <> rhs
  Minus → lhs <> " - " <> rhs
  Mult → lhs <> " * " <> rhs
  Div → lhs <> " / " <> rhs
  Mod → lhs <> " % " <> rhs
  Pow → lhs <> " ^ " <> rhs
  In → lhs <> " IN " <> rhs
  FieldDeref → lhs <> "." <> rhs
  IndexDeref → lhs <> "[" <> rhs <> "]"
  Limit → lhs <> " LIMIT " <> rhs
  Offset → lhs <> " OFFSET " <> rhs
  Sample → lhs <> " SAMPLE " <> rhs
  Union → lhs <> " UNION " <> rhs
  UnionAll → lhs <> " UNION ALL " <> rhs
  Intersect → lhs <> " INTERSECT " <> rhs
  IntersectAll → lhs <> " INTERSECT ALL " <> rhs
  Except → lhs <> " EXCEPT " <> rhs
  UnshiftMap → "{" <> lhs <> ": " <> rhs <> "...}"

printBinaryOperatorPretty ∷ String → String → BinaryOperator → String
printBinaryOperatorPretty lhs rhs = case _ of
  UnionAll → lhs <> "\nUNION ALL\n" <> rhs
  other → printBinaryOperator lhs rhs other
