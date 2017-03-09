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

import Data.Eq (class Eq1, eq1)
import Data.Foldable as F
import Data.Traversable as T
import Data.Functor.Mu (Mu)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Ord (class Ord1, compare1)

import Data.Json.Extended.Signature (EJsonF, renderEJsonF)

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

import Matryoshka (Algebra, cata)

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

data SqlF literal a
  = SetLiteral (L.List a)
  | Literal (literal a)
  | Splice (Maybe a)
  | Binop (BinopR a)
  | Unop (UnopR a)
  | Ident String
  | InvokeFunction (InvokeFunctionR a)
  | Match (MatchR a)
  | Switch (SwitchR a)
  | Let (LetR a)
  | Vari String
  | Select (SelectR a)
  | Parens a

derive instance eqSqlF ∷ (Eq a, Eq (l a)) ⇒ Eq (SqlF l a)
derive instance ordSqlF ∷ (Ord a, Ord (l a)) ⇒ Ord (SqlF l a)

instance eq1SqlF ∷ Eq1 l ⇒ Eq1 (SqlF l) where
  eq1 (Literal l) (Literal ll) = eq1 l ll
  eq1 (Splice a) (Splice aa) = eq a aa
  eq1 (Binop r) (Binop rr) =
    r.lhs == rr.lhs
    && r.rhs == rr.rhs
    && r.op == rr.op
  eq1 (Unop r) (Unop rr) =
    r.expr == rr.expr
    && r.op == rr.op
  eq1 (Ident s) (Ident ss) =
    s == ss
  eq1 (InvokeFunction r) (InvokeFunction rr) =
    r.name == rr.name
    && r.args == rr.args
  eq1 (Match r) (Match rr) =
    r.else_ == rr.else_
    && r.cases == rr.cases
    && r.expr == rr.expr
  eq1 (Switch r) (Switch rr) =
    r.cases == rr.cases
    && r.else_ == rr.else_
  eq1 (Let r) (Let rr) =
    r.in_ == r.in_
    && r.bindTo == rr.bindTo
    && r.ident == rr.ident
  eq1 (Vari v) (Vari vv) =
    v == vv
  eq1 (Parens a) (Parens aa) =
    a == aa
  eq1 (Select r) (Select rr) =
    r.isDistinct == rr.isDistinct
    && r.projections == rr.projections
    && r.relations == rr.relations
    && r.filter == rr.filter
    && r.groupBy == rr.groupBy
    && r.orderBy == rr.orderBy
  eq1 _ _ = false

instance ord1SqlF ∷ Ord1 l ⇒ Ord1 (SqlF l) where
  compare1 (Literal l) (Literal ll) = compare1 l ll
  compare1 (Literal _) _ = LT
  compare1 _ (Literal _) = GT
  compare1 (SetLiteral s) (SetLiteral ss) = compare s ss
  compare1 (SetLiteral _) _ = LT
  compare1 _ (SetLiteral _) = GT
  compare1 (Splice a) (Splice aa) = compare a aa
  compare1 (Splice _) _ = LT
  compare1 _ (Splice _) = GT
  compare1 (Binop r) (Binop rr) =
    compare r.lhs rr.lhs
    <> compare r.rhs rr.rhs
    <> compare r.op rr.op
  compare1 (Binop _) _ = LT
  compare1 _ (Binop _) = GT
  compare1 (Unop r) (Unop rr) =
    compare r.op rr.op
    <> compare r.expr rr.expr
  compare1 (Unop _) _ = LT
  compare1 _ (Unop _) = GT
  compare1 (Ident s) (Ident ss) = compare s ss
  compare1 (Ident s) _ = LT
  compare1 _ (Ident s) = GT
  compare1 (InvokeFunction r) (InvokeFunction rr) =
    compare r.name rr.name
    <> compare r.args rr.args
  compare1 (InvokeFunction _) _ = LT
  compare1 _ (InvokeFunction _) = GT
  compare1 (Match r) (Match rr) =
    compare r.else_ rr.else_
    <> compare r.expr rr.expr
    <> compare r.cases rr.cases
  compare1 (Match _) _ = LT
  compare1 _ (Match _) = GT
  compare1 (Switch r) (Switch rr) =
    compare r.else_ rr.else_
    <> compare r.cases rr.cases
  compare1 (Switch _) _ = LT
  compare1 _ (Switch _) = GT
  compare1 (Let r) (Let rr) =
    compare r.in_ rr.in_
    <> compare r.bindTo rr.bindTo
    <> compare r.ident rr.ident
  compare1 (Let _) _ = LT
  compare1 _ (Let _) = GT
  compare1 (Vari v) (Vari vv) = compare v vv
  compare1 (Vari _) _ = LT
  compare1 _ (Vari _) = GT
  compare1 (Parens a) (Parens aa) = compare a aa
  compare1 (Parens a) _ = LT
  compare1 _ (Parens _) = GT
  compare1 (Select r) (Select rr) =
    compare r.isDistinct rr.isDistinct
    <> compare r.projections rr.projections
    <> compare r.filter rr.filter
    <> compare r.relations rr.relations
    <> compare r.orderBy rr.orderBy
    <> compare r.groupBy rr.groupBy

instance functorAST ∷ Functor l ⇒ Functor (SqlF l) where
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
    Let { ident, bindTo, in_ } →
      Let { ident
          , bindTo: f bindTo
          , in_: f in_
          }
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
    Literal l →
      Literal $ map f l
    Parens t →
      Parens $ f t



instance foldableSqlF ∷ F.Foldable l ⇒ F.Foldable (SqlF l) where
  foldMap f = case _ of
    Ident _ → mempty
    SetLiteral lst → F.foldMap f lst
    Splice mbA → F.foldMap f mbA
    Binop { lhs, rhs } → f lhs <> f rhs
    Unop { expr } → f expr
    InvokeFunction { args } → F.foldMap f args
    Match { expr, cases, else_ } → f expr <> F.foldMap (F.foldMap f) cases <> F.foldMap f else_
    Switch { cases, else_} → F.foldMap (F.foldMap f) cases <> F.foldMap f else_
    Let { bindTo, in_ } → f bindTo <> f in_
    Vari _ → mempty
    Select { projections, relations, filter, groupBy, orderBy } →
      F.foldMap (F.foldMap f) projections
      <> F.foldMap (F.foldMap f) relations
      <> F.foldMap f filter
      <> F.foldMap (F.foldMap f) groupBy
      <> F.foldMap (F.foldMap f) orderBy
    Parens a → f a
    Literal l → F.foldMap f l
  foldl f a = case _ of
    Ident _ → a
    SetLiteral lst → F.foldl f a lst
    Splice mbA → F.foldl f a mbA
    Binop { lhs, rhs } → f (f a lhs) rhs
    Unop { expr } → f a expr
    InvokeFunction { args } → F.foldl f a args
    Match { expr, cases, else_ } →
      F.foldl f (F.foldl (F.foldl f) (f a expr) cases) else_
    Switch { cases, else_ } →
      F.foldl f (F.foldl (F.foldl f) a cases) else_
    Let { bindTo, in_} →
      f (f a bindTo) in_
    Vari _ → a
    Select { projections, relations, filter, groupBy, orderBy } →
      F.foldl (F.foldl f)
      (F.foldl (F.foldl f)
       (F.foldl f
        (F.foldl (F.foldl f)
         (F.foldl (F.foldl f) a
          projections)
         relations)
        filter)
       groupBy)
      orderBy
    Parens p → f a p
    Literal l → F.foldl f a l
  foldr f a = case _ of
    Ident _ → a
    SetLiteral lst → F.foldr f a lst
    Splice mbA → F.foldr f a mbA
    Binop { lhs, rhs } → f rhs $ f lhs a
    Unop { expr } → f expr a
    InvokeFunction { args } → F.foldr f a args
    Match { expr, cases, else_ } →
      F.foldr f (F.foldr (flip $ F.foldr f) (f expr a) cases) else_
    Switch { cases, else_ } →
      F.foldr f (F.foldr (flip $ F.foldr f) a cases) else_
    Let { bindTo, in_ } →
      f bindTo $ f in_ a
    Vari _ → a
    Select { projections, relations, filter, groupBy, orderBy } →
      F.foldr (flip $ F.foldr f)
      (F.foldr (flip $ F.foldr f)
       (F.foldr f
        (F.foldr (flip $ F.foldr f)
         (F.foldr (flip $ F.foldr f) a
          projections)
         relations)
        filter)
       groupBy)
      orderBy
    Parens p → f p a
    Literal l → F.foldr f a l



instance traversableSqlF ∷ T.Traversable l ⇒ T.Traversable (SqlF l) where
  traverse f = case _ of
    SetLiteral lst → map SetLiteral $ T.traverse f lst
    Literal l → map Literal $ T.traverse f l
    Splice mbA → map Splice $ T.traverse f mbA
    Binop { lhs, rhs, op } →
      map Binop $ { lhs: _, rhs: _, op } <$> f lhs <*> f rhs
    Unop { op, expr } →
      map Unop $ { expr: _, op } <$> f expr
    Ident s → pure $ Ident s
    InvokeFunction { name, args } →
      map InvokeFunction $ { name, args:_ } <$> T.traverse f args
    Match { expr, cases, else_ } →
      map Match
      $ { expr: _, cases: _, else_: _ }
      <$> f expr
      <*> T.traverse (T.traverse f) cases
      <*> T.traverse f else_
    Switch { cases, else_ } →
      map Switch
      $ { cases: _, else_: _ }
      <$> T.traverse (T.traverse f) cases
      <*> T.traverse f else_
    Let { bindTo, in_, ident } →
      map Let
      $ { bindTo: _, in_: _, ident }
      <$> f bindTo
      <*> f in_
    Vari s → pure $ Vari s
    Parens p → map Parens $ f p
    Select { isDistinct, projections, relations, filter, groupBy, orderBy } →
      map Select
      $ { isDistinct, projections: _, relations: _, filter: _, groupBy: _, orderBy: _}
      <$> T.traverse (T.traverse f) projections
      <*> T.traverse (T.traverse f) relations
      <*> T.traverse f filter
      <*> T.traverse (T.traverse f) groupBy
      <*> T.traverse (T.traverse f) orderBy
  sequence = T.sequenceDefault

printF ∷ ∀ l. Algebra l String → Algebra (SqlF l) String
printF printLiteralF = case _ of
  Splice Nothing → "*"
  Splice (Just s) → s <> ".*"
  SetLiteral lst → "(" <> F.intercalate ", " lst <> ")"
  Literal l → printLiteralF l
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

type Sql = Mu (SqlF EJsonF)

print ∷ Sql → String
print = cata (printF renderEJsonF)
