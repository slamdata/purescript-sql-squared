module SqlSquare.AST
  ( BinopR
  , UnopR
  , InvokeFunctionR
  , MatchR
  , SwitchR
  , LetR
  , SelectR
  , SqlF(..)
  , Sql
  , printF
  , print
  , module SqlSquare.Utils
  , module OT
  , module JT
  , module SqlSquare.BinaryOperator
  , module SqlSquare.UnaryOperator
  , module SqlSquare.GroupBy
  , module SqlSquare.Case
  , module SqlSquare.OrderBy
  , module SqlSquare.Projection
  , module SqlSquare.Relation
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Eq (class Eq1)
import Data.Foldable as F
import Data.Functor.Mu (Mu)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1)
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX

import SqlSquare.Utils (type (×), (×), (∘), (⋙))
import SqlSquare.OrderType as OT
import SqlSquare.JoinType as JT
import SqlSquare.BinaryOperator (BinaryOperator(..))
import SqlSquare.UnaryOperator (UnaryOperator(..))
import SqlSquare.GroupBy (GroupBy(..), printGroupBy)
import SqlSquare.Case (Case(..), printCase)
import SqlSquare.OrderBy (OrderBy(..), printOrderBy)
import SqlSquare.Projection (Projection(..), printProjection)
import SqlSquare.Relation (Relation(..), printRelation, FUPath, JoinRelR, ExprRelR, TableRelR, VariRelR, IdentRelR)

import Matryoshka (class Recursive, Algebra, cata)

type BinopR a =
  { lhs ∷ a
  , rhs ∷ a
  , op ∷ BinaryOperator
  }

type UnopR a =
  { expr ∷ a
  , op ∷ UnaryOperator
  }

type InvokeFunctionR a =
  { name ∷ String
  , args ∷ L.List a
  }

type MatchR a =
  { expr ∷ a
  , cases ∷ L.List (Case a)
  , else_ ∷ Maybe a
  }

type SwitchR a =
  { cases ∷ L.List (Case a)
  , else_ ∷ Maybe a
  }

type LetR a =
  { ident ∷ String
  , bindTo ∷ a
  , in_ ∷ a
  }

type SelectR a =
  { isDistinct ∷  Boolean
  , projections ∷ L.List (Projection a)
  , relations ∷ Maybe (Relation a)
  , filter ∷ Maybe a
  , groupBy ∷ Maybe (GroupBy a)
  , orderBy ∷ Maybe (OrderBy a)
  }

data SqlF a
  = SetLiteral (L.List a)
  | ArrayLiteral (L.List a)
  | MapLiteral (L.List (a × a))
  | Splice (Maybe a)
  | Binop (BinopR a)
  | Unop (UnopR a)
  | Ident String
  | InvokeFunction (InvokeFunctionR a)
  | Match (MatchR a)
  | Switch (SwitchR a)
  | Let (LetR a)
  | IntLiteral Int
  | FloatLiteral Number
  | StringLiteral String
  | NullLiteral
  | BoolLiteral Boolean
  | Vari String
  | Select (SelectR a)
  | Parens a

derive instance eqSqlF ∷ Eq a ⇒ Eq (SqlF a)
derive instance ordSqlF ∷ Ord a ⇒ Ord (SqlF a)

instance eq1SqlF ∷ Eq1 SqlF where
  eq1 = eq
instance ord1SqlF ∷ Ord1 SqlF where
  compare1 = compare

instance functorAST ∷ Functor SqlF where
  map f = case _ of
    Select { isDistinct, projections, relations, filter, groupBy, orderBy } →
      Select { isDistinct
             , projections: map (map f) projections
             , relations: map (map f) relations
             , filter: map f filter
             , groupBy: map (map f) groupBy
             , orderBy: map (map f) orderBy
             }
    Vari s →
      Vari s
    BoolLiteral b →
      BoolLiteral b
    NullLiteral →
      NullLiteral
    StringLiteral s →
      StringLiteral s
    FloatLiteral n →
      FloatLiteral n
    IntLiteral i →
      IntLiteral i
    Let { ident, bindTo, in_ } →
      Let { ident
          , bindTo: f bindTo
          , in_: f in_
          }
    MapLiteral lst →
      MapLiteral $ map (bimap f f) lst
    Splice a →
      Splice $ map f a
    Binop { lhs, rhs, op } →
      Binop { lhs: f lhs
            , rhs: f rhs
            , op
            }
    Unop { expr, op } →
      Unop { expr: f expr
           , op
           }
    Ident s →
      Ident s
    InvokeFunction { name, args } →
      InvokeFunction { name
                     , args: map f args
                     }
    Match { expr, cases, else_ } →
      Match { expr: f expr
            , cases: map (map f) cases
            , else_: map f else_
            }
    Switch { cases, else_ } →
      Switch { cases: map (map f) cases
             , else_: map f else_
             }
    SetLiteral lst →
      SetLiteral $ map f lst
    ArrayLiteral lst →
      ArrayLiteral $ map f lst
    Parens t →
      Parens $ f t


printF ∷ Algebra SqlF String
printF = case _ of
  SetLiteral lst → "(" <> F.intercalate ", " lst <> ")"
  ArrayLiteral lst → "[" <> F.intercalate ", " lst <> "]"
  MapLiteral tplLst → "{" <> F.intercalate ", " (map (\(k × v) → k <> ": " <> v) tplLst) <> "}"
  Splice mb → case mb of
    Nothing → "*"
    Just a → a <> ".*"
  Binop {lhs, rhs, op} → case op of
    IfUndefined → lhs <> " ?? " <> rhs
    Range → lhs <> " .. " <> rhs
    Or → lhs <> " or " <> rhs
    And → lhs <> " and " <> rhs
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
    In → lhs <> " in " <> rhs
    FieldDeref → lhs <> "." <> rhs
    IndexDeref → lhs <> "[" <> rhs <> "]"
    Limit → lhs <> " limit " <> rhs
    Offset → lhs <> " offset " <> rhs
    Sample → lhs <> " sample " <> rhs
    Union → lhs <> " union " <> rhs
    UnionAll → lhs <> " union all " <> rhs
    Intersect → lhs <> " intersect " <> rhs
    IntersectAll → lhs <> " intersect all " <> rhs
    Except → lhs <> " except " <> rhs
    UnshiftMap → "{" <> lhs <> ": " <> rhs <> "...}"
  Unop {expr, op} → case op of
    Not → "not " <> expr
    Exists → "exists " <> expr
    Positive → "+" <> expr
    Negative → "-" <> expr
    Distinct → "distinct " <> expr
    FlattenMapKeys → expr <> "{*: }"
    FlattenMapValues → expr <> "{*}"
    ShiftMapKeys → expr <> "{_: }"
    ShiftMapValues → expr <> "{_}"
    FlattenArrayIndices → expr <> "[*:]"
    FlattenArrayValues → expr <> "[*]"
    ShiftArrayIndices → expr <> "[_:]"
    ShiftArrayValues → expr <> "[_]"
    UnshiftArray → "[" <> expr <> "...]"
  Ident s →
    "`" <> s <> "`"
  InvokeFunction {name, args} →
    name <> "(" <> F.intercalate "," args <> ")"
  Match { expr, cases, else_ } →
    "case "
    <> expr
    <> F.intercalate " " (map printCase cases)
    <> F.foldMap (" else " <> _) else_
  Switch { cases, else_ } →
    "case "
    <> F.intercalate " " (map printCase cases)
    <> F.foldMap (" else " <> _) else_
  Let { ident, bindTo, in_ } →
    ident <> " := " <> bindTo <> "; " <> in_
  IntLiteral int →
    show int
  FloatLiteral n →
    show n
  StringLiteral s →
    renderString s
  NullLiteral →
    "null"
  BoolLiteral b →
    show b
  Vari s →
    ":" <> s
  Select { isDistinct, projections, relations, filter, groupBy, orderBy } →
    "select "
    <> (if isDistinct then "distinct " else "")
    <> (F.intercalate ", " $ map printProjection projections)
    <> (relations # F.foldMap \rs →
         " from " <> printRelation rs)
    <> (filter # F.foldMap \f → " where " <> f)
    <> (groupBy # F.foldMap \gb → " group by " <> printGroupBy gb)
    <> (orderBy # F.foldMap \ob → " order by " <> printOrderBy ob)
  Parens t →
    "(" <> t <> ")"
  where
  replaceAll
    ∷ String
    → String
    → String
    → String
  replaceAll i =
      RX.replace $ URX.unsafeRegex i RXF.global

  renderString
    ∷ String
    → String
  renderString str =
    "\"" <> replaceAll "\"" "\\\"" str <> "\""

type Sql = Mu SqlF

print ∷ ∀ t. Recursive t SqlF ⇒ t → String
print = cata printF
