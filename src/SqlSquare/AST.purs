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
  , encodeJson
  , decodeJson
  , arbitrarySqlOfSize
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

import Control.Alt ((<|>))
import Control.Lazy as Lazy

import Data.Array as A
import Data.Argonaut as J
import Data.Either as E
import Data.Eq (class Eq1, eq1)
import Data.Int as Int
import Data.Foldable as F
import Data.Traversable as T
import Data.Functor.Mu (Mu)
import Data.List as L
import Data.NonEmpty as NE
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (mempty)
import Data.Ord (class Ord1, compare1)
import Data.String as S

import Data.Json.Extended.Signature (EJsonF, renderEJsonF, encodeJsonEJsonF, decodeJsonEJsonF, arbitraryEJsonF, parseEJsonF)

import SqlSquare.Utils (type (×), (×), (∘), (⋙))
import SqlSquare.OrderType as OT
import SqlSquare.JoinType as JT
import SqlSquare.BinaryOperator (BinaryOperator(..))
import SqlSquare.UnaryOperator (UnaryOperator(..))
import SqlSquare.GroupBy
  ( GroupBy(..)
  , printGroupBy
  , arbitraryGroupBy
  , encodeJsonGroupBy
  , decodeJsonGroupBy)
import SqlSquare.Case
  ( Case(..)
  , printCase
  , arbitraryCase
  , encodeJsonCase
  , decodeJsonCase)
import SqlSquare.OrderBy
  ( OrderBy(..)
  , printOrderBy
  , arbitraryOrderBy
  , encodeJsonOrderBy
  , decodeJsonOrderBy)
import SqlSquare.Projection
  ( Projection(..)
  , printProjection
  , arbitraryProjection
  , encodeJsonProjection
  , decodeJsonProjection)
import SqlSquare.Relation
  ( Relation(..)
  , printRelation
  , arbitraryRelation
  , FUPath
  , JoinRelR
  , ExprRelR
  , TableRelR
  , VariRelR
  , IdentRelR
  , encodeJsonRelation
  , decodeJsonRelation)
import SqlSquare.OrderType (OrderType(..))

import Matryoshka (Algebra, cata, CoalgebraM, anaM, embed)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

import Unsafe.Coerce (unsafeCoerce)

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
  eq1 (SetLiteral lst) (SetLiteral llst) = eq lst llst
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

derive instance functorAST ∷ Functor l ⇒ Functor (SqlF l)

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
  Unop {expr, op} → case op of
    Not → "NOT " <> expr
    Exists → "EXISTS " <> expr
    Positive → "+" <> expr
    Negative → "-" <> expr
    Distinct → "DISTINCT " <> expr
    FlattenMapKeys → expr <> "{*:}"
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
    "CASE "
    <> expr
    <> F.intercalate " " (map printCase cases)
    <> F.foldMap (" ELSE " <> _) else_
  Switch { cases, else_ } →
    "CASE "
    <> F.intercalate " " (map printCase cases)
    <> F.foldMap (" ELSE " <> _) else_
  Let { ident, bindTo, in_ } →
    ident <> " := " <> bindTo <> "; " <> in_
  Vari s →
    ":" <> s
  Select { isDistinct, projections, relations, filter, groupBy, orderBy } →
    "SELECT "
    <> (if isDistinct then "DISTINCT " else "")
    <> (F.intercalate ", " $ map printProjection projections)
    <> (relations # F.foldMap \rs →
         " FROM " <> printRelation rs)
    <> (filter # F.foldMap \f → " WHERE " <> f)
    <> (groupBy # F.foldMap \gb → " GROUP BY " <> printGroupBy gb)
    <> (orderBy # F.foldMap \ob → " ORDER BY " <> printOrderBy ob)
  Parens t →
    "(" <> t <> ")"

type Sql = Mu (SqlF EJsonF)

print ∷ Sql → String
print = cata (printF renderEJsonF)

encodeJsonSqlF ∷ ∀ l. Algebra l J.Json → Algebra (SqlF l) J.Json
encodeJsonSqlF alg = case _ of
  SetLiteral lst →
    "tag" J.:= "set literal"
    J.~> "value" J.:= lst
    J.~> J.jsonEmptyObject
  Literal l →
    "tag" J.:= "literal"
    J.~> "value" J.:= alg l
    J.~> J.jsonEmptyObject
  Splice a →
    "tag" J.:= "splice"
    J.~> "value" J.:= a
    J.~> J.jsonEmptyObject
  Binop { lhs, rhs, op } →
    "tag" J.:= "binop"
    J.~> "lhs" J.:= lhs
    J.~> "rhs" J.:= rhs
    J.~> "op" J.:= op
    J.~> J.jsonEmptyObject
  Unop { expr, op } →
    "tag" J.:= "unop"
    J.~> "expr" J.:= expr
    J.~> "op" J.:= op
    J.~> J.jsonEmptyObject
  Ident s →
    "tag" J.:= "ident"
    J.~> "value" J.:= s
    J.~> J.jsonEmptyObject
  InvokeFunction { name, args } →
    "tag" J.:= "invoke function"
    J.~> "name" J.:= name
    J.~> "args" J.:= args
    J.~> J.jsonEmptyObject
  Match { expr, cases, else_ } →
    "tag" J.:= "match"
    J.~> "expr" J.:= expr
    J.~> "cases" J.:= map encodeJsonCase cases
    J.~> "else_" J.:= else_
    J.~> J.jsonEmptyObject
  Switch { cases, else_ } →
    "tag" J.:= "switch"
    J.~> "cases" J.:= map encodeJsonCase cases
    J.~> "else_" J.:= else_
    J.~> J.jsonEmptyObject
  Let { ident, bindTo, in_ } →
    "tag" J.:= "let"
    J.~> "ident" J.:= ident
    J.~> "bindTo" J.:= bindTo
    J.~> "in_" J.:= in_
    J.~> J.jsonEmptyObject
  Vari s →
    "tag" J.:= "vari"
    J.~> "value" J.:= s
    J.~> J.jsonEmptyObject
  Select { isDistinct, projections, relations, filter, groupBy, orderBy } →
    "tag" J.:= "select"
    J.~> "isDistinct" J.:= isDistinct
    J.~> "projections" J.:= map encodeJsonProjection projections
    J.~> "relations" J.:= map encodeJsonRelation relations
    J.~> "filter" J.:= filter
    J.~> "groupBy" J.:= map encodeJsonGroupBy groupBy
    J.~> "orderBy" J.:= map encodeJsonOrderBy orderBy
    J.~> J.jsonEmptyObject
  Parens a →
    "tag" J.:= "parens"
    J.~> "value" J.:= a
    J.~> J.jsonEmptyObject


decodeJsonSqlF
  ∷ ∀ l
  . CoalgebraM (E.Either String) l J.Json
  → CoalgebraM (E.Either String) (SqlF l) J.Json
decodeJsonSqlF coalg = J.decodeJson >=> \obj → do
  tag ← obj J..? "tag"
  case tag of
    "set literal" → decodeSetLiteral obj
    "literal" → decodeLiteral obj
    "splice" → decodeSplice obj
    "binop" → decodeBinop obj
    "unop" → decodeUnop obj
    "ident" → decodeIdent obj
    "invoke function" → decodeInvokeFunction obj
    "match" → decodeMatch obj
    "switch" → decodeSwitch obj
    "let" → decodeLet obj
    "vari" → decodeVari obj
    "select" → decodeSelect obj
    "parens" → decodeParens obj
    _ → E.Left "This is not SqlF expression"
  where
  decodeSetLiteral obj = do
    v ← obj J..? "value"
    pure $ SetLiteral v

  decodeLiteral obj = do
    v ← obj J..? "value"
    literal ← coalg v
    pure $ Literal literal

  decodeSplice obj = do
    v ← obj J..? "value"
    pure $ Splice v

  decodeBinop obj = do
    lhs ← obj J..? "lhs"
    rhs ← obj J..? "rhs"
    op ← obj J..? "op"
    pure $ Binop { lhs, rhs, op }

  decodeUnop obj = do
    expr ← obj J..? "expr"
    op ← obj J..? "op"
    pure $ Unop { expr, op }

  decodeIdent obj = do
    v ← obj J..? "value"
    pure $ Ident v

  decodeInvokeFunction obj = do
    name ← obj J..? "name"
    args ← obj J..? "args"
    pure $ InvokeFunction { name, args }

  decodeMatch obj = do
    expr ← obj J..? "expr"
    cases ← (obj J..? "cases") >>= T.traverse decodeJsonCase
    else_ ← obj J..? "else_"
    pure $ Match { expr, cases, else_ }

  decodeSwitch obj = do
   cases ← (obj J..? "cases") >>= T.traverse decodeJsonCase
   else_ ← obj J..? "else_"
   pure $ Switch { cases, else_ }

  decodeLet obj = do
    ident ← obj J..? "ident"
    bindTo ← obj J..? "bindTo"
    in_ ← obj J..? "in_"
    pure $ Let { ident, bindTo, in_ }

  decodeVari obj = do
    v ← obj J..? "value"
    pure $ Vari v

  decodeSelect obj = do
    isDistinct ← obj J..? "isDistinct"
    projections ← (obj J..? "projections") >>= T.traverse decodeJsonProjection
    relations ← (obj J..? "relations") >>= T.traverse decodeJsonRelation
    filter ← obj J..? "filter"
    groupBy ← (obj J..? "groupBy") >>= T.traverse decodeJsonGroupBy
    orderBy ← (obj J..? "orderBy") >>= T.traverse decodeJsonOrderBy
    pure $ Select { isDistinct, projections, relations, filter, groupBy, orderBy }

  decodeParens obj = do
    v ← obj J..? "value"
    pure $ Parens v

encodeJson ∷ Sql → J.Json
encodeJson = cata $ encodeJsonSqlF encodeJsonEJsonF

decodeJson ∷ J.Json → E.Either String Sql
decodeJson = anaM $ decodeJsonSqlF decodeJsonEJsonF

arbitrarySqlF
  ∷ ∀ l
  . CoalgebraM Gen.Gen l Int
  → CoalgebraM Gen.Gen (SqlF l) Int
arbitrarySqlF genLiteral n
  | n < 2 =
  Gen.oneOf (map Literal $ genLiteral n)
    [ map Ident SC.arbitrary
    , map Vari SC.arbitrary
    , pure $ Splice Nothing
    , pure $ SetLiteral L.Nil
    ]
  | otherwise = do
  Gen.oneOf (map Literal $ genLiteral n)
    [ pure $ Splice $ Just $ n - 1
    , pure $ Parens $ n - 1
    , genSetLiteral
    , genBinop
    , genUnop
    , genInvokeFunction
    , genMatch
    , genSwitch
    , genLet
    , genSelect
    ]
  where
  genSetLiteral = do
    len ← Gen.chooseInt 0 $ n - 1
    pure $ SetLiteral $ map (const $ n - 1) $ L.range 0 len

  genBinop = do
    op ← SC.arbitrary
    pure $ Binop { op, lhs: n - 1, rhs: n - 1 }

  genUnop = do
    op ← SC.arbitrary
    pure $ Unop { op, expr: n - 1 }

  genInvokeFunction = do
    name ← SC.arbitrary
    len ← Gen.chooseInt 0 $ n - 1
    pure $ InvokeFunction { name, args: map (const $ n - 1) $ L.range 0 len }

  genMatch = do
    nothing ← SC.arbitrary
    len ← Gen.chooseInt 0 $ n - 1
    let
      foldFn acc _ = do
        cs ← arbitraryCase $ n - 1
        pure $ cs L.: acc
    cases ← L.foldM foldFn L.Nil $ L.range 0 len
    pure $ Match { expr: n - 1
                 , cases
                 , else_: if nothing then Nothing else Just $ n - 1
                 }
  genSwitch = do
    nothing ← SC.arbitrary
    len ← Gen.chooseInt 0 $ n - 1
    let
      foldFn acc _ = do
        cs ← arbitraryCase $ n - 1
        pure $ cs L.: acc
    cases ← L.foldM foldFn L.Nil $ L.range 0 len
    pure $ Switch { cases
                  , else_: if nothing then Nothing else Just $ n - 1
                  }

  genLet = do
    ident ← map (Int.toStringAs Int.hexadecimal) $ SC.arbitrary
    pure $ Let { ident
               , bindTo: n - 1
               , in_: n - 1
               }
  genSelect = do
    prjLen ← Gen.chooseInt 0 $ n - 1
    mbRelation ← SC.arbitrary
    mbFilter ← SC.arbitrary
    mbGroupBy ← SC.arbitrary
    mbOrderBy ← SC.arbitrary

    isDistinct ← SC.arbitrary

    let
      foldPrj acc _ = do
        prj ← arbitraryProjection $ n - 1
        pure $ prj L.:acc
    projections ←
      L.foldM foldPrj L.Nil $ L.range 0 prjLen

    relations ←
      if mbRelation
        then pure Nothing
        else map Just $ arbitraryRelation $ n - 1

    groupBy ←
      if mbGroupBy
        then pure Nothing
        else map Just $ arbitraryGroupBy $ n - 1

    orderBy ←
      if mbOrderBy
        then pure Nothing
        else map Just $ arbitraryOrderBy $ n - 1

    pure $ Select { isDistinct
                  , projections
                  , relations
                  , filter: if mbFilter then Nothing else Just $ n - 1
                  , groupBy
                  , orderBy
                  }

arbitrarySqlOfSize ∷ Int → Gen.Gen Sql
arbitrarySqlOfSize = anaM $ arbitrarySqlF arbitraryEJsonF

parseSql ∷ ∀ m. Monad m ⇒ P.ParserT String m Sql
parseSql = Lazy.fix $ \f → map embed $ parseSqlF parseEJsonF f

parseSqlF
  ∷ ∀ m a l
  . Monad m
  ⇒ (P.ParserT String m a → P.ParserT String m (l a))
  → P.ParserT String m a
  → P.ParserT String m (SqlF l a)
parseSqlF litParserCb cb =
  (PC.try $ parseLiteral cb)
  <|> (PC.try $ parseLet cb)
  <|> (PC.try $ parseSelect cb)
  <|> (PC.try parseVariable)
  <|> (PC.try $ parseSplice cb)
  <|> (PC.try $ parseIdent)
  <|> (PC.try $ parseInvokeFunction cb)
  <|> (PC.try $ parseParens cb)
  <|> (PC.try $ parseUnaryOperator cb)
  <|> (PC.try $ parseBinaryOperator cb)
  <|> (PC.try $ parseSwitch cb)

  where
  parseLiteral r = map Literal $ litParserCb r

  parseLet r = do
    i ← identParser
    PS.string ":="
    bindTo ← r
    PS.string ";"
    in_ ← r
    pure $ Let { ident: i, bindTo, in_ }

  parseSelect r = do
    PS.string "select"
    PS.skipSpaces
    isDistinct ← map isJust $ PC.optionMaybe (PS.string "distinct")
    projections ← flip PC.sepBy (PS.string ",") do
      PS.skipSpaces
      parseProjection r

    relations ← PC.optionMaybe do
      PS.skipSpaces
      PS.string "from"
      PS.skipSpaces
      parseRelation r
    filter ← PC.optionMaybe do
      PS.string "where"
      PS.skipSpaces
      r
    groupBy ← PC.optionMaybe do
      PS.string "group by"
      PS.skipSpaces
      parseGroupBy r
    orderBy ← PC.optionMaybe do
      PS.string "order by"
      PS.skipSpaces
      parseOrderBy r
    pure $ Select { isDistinct, projections, relations, filter, groupBy, orderBy }

  parseProjection r = do
    expr ← r
    alias ← PC.optionMaybe do
      PS.skipSpaces
      PS.string "as"
      PS.skipSpaces
      identParser
    pure $ Projection { expr, alias }

  parseRelation r = do
    (PC.try $ parseExprRelation r)
    <|> (PC.try $ parseVariRelation)
    <|> (PC.try $ parseTableRelation)
    <|> (PC.try $ parseJoinRelation r)
    <|> (parseIdentRelation)

  parseExprRelation r = do
    PS.string "("
    PS.skipSpaces
    expr ← r
    PS.skipSpaces
    aliasName ← identParser
    pure $ ExprRelation { expr, aliasName }

  parseVariRelation = do
    PS.string ":"
    vari ← identParser
    PS.skipSpaces
    alias ← PC.optionMaybe identParser
    pure $ VariRelation { vari, alias }

  parseIdentRelation = do
    ident ← identParser
    PS.skipSpaces
    alias ← PC.optionMaybe identParser
    pure $ IdentRelation { ident, alias }

  parseTableRelation = do
    ident ← identParser
    PS.skipSpaces
    alias ← PC.optionMaybe identParser
    pure $ TableRelation { tablePath: unsafeCoerce unit, alias }

  parseJoinRelation r = do
    left ← parseRelation r
    PS.skipSpaces
    joinType ← parseJoinType
    right ← parseRelation r
    clause ← r
    pure $ JoinRelation { left, right, joinType, clause }

  parseJoinType = do
    ((PC.try $ PS.string "left join") $> JT.LeftJoin)
    <|> ((PC.try $ PS.string "right join") $> JT.RightJoin)
    <|> ((PC.try $ PS.string "full join") $> JT.FullJoin)
    <|> ((PC.try $ PS.string "inner join") $> JT.InnerJoin)
    <|> (PS.string "join" $> JT.FullJoin)

  parseGroupBy r = do
    keys ← PC.sepBy1 r $ PS.skipSpaces *> PS.string ","
    PS.skipSpaces
    having ← PC.optionMaybe do
      PS.string "having"
      PS.skipSpaces
      r
    pure $ GroupBy { keys, having }

  parseOrderBy ∷ P.ParserT String m a → P.ParserT String m (OrderBy a)
  parseOrderBy r = do
    head ← parseOneOB r
    PS.skipSpaces
    PC.optional $ PS.string ","
    PS.skipSpaces
    tail ← PC.sepBy (parseOneOB r) $ PS.skipSpaces *> PS.string ","
    pure $ OrderBy $ head NE.:| tail

  parseOneOB r = do
    ot ← parseOrderType
    v ← r
    pure $ ot × v

  parseOrderType =
    (PS.string "ASC" $> ASC)
    <|> (PS.string "DESC" $> DESC)

  parseVariable = do
    PS.string ":"
    ident ← identParser
    pure $ Vari ident

  parseSplice r = do
    PS.string "*"
    mbTail ← PC.optionMaybe do
      PS.string "."
      r
    pure $ Splice mbTail

  parseIdent = map Ident identParser

  parseInvokeFunction r = do
    name ← identParser
    PS.string "("
    args ← PC.sepBy r $ PS.string ","
    PS.string ")"
    pure $ InvokeFunction { name, args }

  parseParens r = do
    PS.string "("
    lst ← PC.sepBy r $ PS.string ","
    case lst of
      L.Cons a L.Nil → pure $ Parens a
      xs → pure $ SetLiteral xs

  parseSwitch r = do
    PS.string "case"
    PS.skipSpaces
    mbExpr ← PC.optionMaybe r
    PS.skipSpaces
    cases ← L.some $ parseCase r
    else_ ← PC.optionMaybe do
      PS.skipSpaces
      PS.string "else"
      PS.skipSpaces
      r
    PS.string "end"
    pure case mbExpr of
      Nothing → Switch { cases, else_ }
      Just expr → Match { expr, cases, else_ }

  parseCase r = do
    PS.skipSpaces
    PS.string "when"
    cond ← r
    PS.skipSpaces
    PS.string "then"
    PS.skipSpaces
    expr ← r
    pure $ Case { cond, expr }

  parseUnaryOperator r = do
    pure $ unsafeCoerce unit

  parseBinaryOperator r = do
    pure $ unsafeCoerce unit


identParser ∷ ∀ m. Monad m ⇒ P.ParserT String m String
identParser =
  map S.fromCharArray
  $ (PC.try $ PC.between (PS.string "`") (PS.string "`") $ A.some stringChar)
  <|> (A.some stringChar)
  where
  stringChar = PS.noneOf [ '`' ]
