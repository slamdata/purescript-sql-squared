module SqlSquared.Signature
  ( BinopR
  , UnopR
  , InvokeFunctionR
  , MatchR
  , SwitchR
  , LetR
  , SelectR
  , FunctionDeclR
  , SqlF(..)
  , SqlDeclF(..)
  , SqlQueryF(..)
  , SqlModuleF(..)
  , printSqlF
  , printSqlDeclF
  , printSqlQueryF
  , printSqlModuleF
  , encodeJsonSqlF
  , encodeJsonSqlDeclF
  , encodeJsonSqlQueryF
  , encodeJsonSqlModuleF
  , decodeJsonSqlF
  , decodeJsonSqlDeclF
  , decodeJsonSqlQueryF
  , decodeJsonSqlModuleF
  , genSqlF
  , genSqlDeclF
  , genSqlQueryF
  , genSqlModuleF
  , module SqlSquared.Utils
  , module OT
  , module JT
  , module BO
  , module UO
  , module GB
  , module CS
  , module OB
  , module PR
  , module RL
  , module ID
  ) where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut as J
import Data.Array as A
import Data.Either as E
import Data.Eq (class Eq1, eq1)
import Data.Foldable as F
import Data.HugeInt as HI
import Data.HugeNum as HN
import Data.Int as Int
import Data.Json.Extended as EJ
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Ord (class Ord1, compare1)
import Data.String as S
import Data.String.Gen as GenS
import Data.Traversable as T
import Matryoshka (Algebra, CoalgebraM, class Corecursive, embed)
import SqlSquared.Signature.BinaryOperator as BO
import SqlSquared.Signature.Case as CS
import SqlSquared.Signature.GroupBy as GB
import SqlSquared.Signature.Ident as ID
import SqlSquared.Signature.JoinType as JT
import SqlSquared.Signature.OrderBy as OB
import SqlSquared.Signature.OrderType as OT
import SqlSquared.Signature.Projection as PR
import SqlSquared.Signature.Relation as RL
import SqlSquared.Signature.UnaryOperator as UO
import SqlSquared.Utils (type (×), (×), (∘), (⋙))

type BinopR a =
  { lhs ∷ a
  , rhs ∷ a
  , op ∷ BO.BinaryOperator
  }

type UnopR a =
  { expr ∷ a
  , op ∷ UO.UnaryOperator
  }

type InvokeFunctionR a =
  { name ∷ String
  , args ∷ L.List a
  }

type MatchR a =
  { expr ∷ a
  , cases ∷ L.List (CS.Case a)
  , else_ ∷ Maybe a
  }

type SwitchR a =
  { cases ∷ L.List (CS.Case a)
  , else_ ∷ Maybe a
  }

type LetR a =
  { ident ∷ String
  , bindTo ∷ a
  , in_ ∷ a
  }

type SelectR a =
  { isDistinct ∷ Boolean
  , projections ∷ L.List (PR.Projection a)
  , relations ∷ Maybe (RL.Relation a)
  , filter ∷ Maybe a
  , groupBy ∷ Maybe (GB.GroupBy a)
  , orderBy ∷ Maybe (OB.OrderBy a)
  }

type FunctionDeclR a =
  { ident ∷ String
  , args ∷ L.List String
  , body ∷ a
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

data SqlDeclF a
  = Import String
  | FunctionDecl (FunctionDeclR a)

newtype SqlModuleF a =
  Module (L.List (SqlDeclF a))

data SqlQueryF a =
  Query (L.List (SqlDeclF a)) a

derive instance eqSqlF ∷ (Eq a, Eq (l a)) ⇒ Eq (SqlF l a)
derive instance ordSqlF ∷ (Ord a, Ord (l a)) ⇒ Ord (SqlF l a)

derive instance eqSqlDeclF ∷ Eq a ⇒ Eq (SqlDeclF a)
derive instance ordSqlDeclF ∷ Ord a ⇒ Ord (SqlDeclF a)

derive instance eqSqlModuleF ∷ Eq a ⇒ Eq (SqlModuleF a)
derive instance ordSqlModuleF ∷ Ord a ⇒ Ord (SqlModuleF a)
derive instance newtypeSqlModuleF ∷ Newtype (SqlModuleF a) _

derive instance eqSqlQueryF ∷ Eq a ⇒ Eq (SqlQueryF a)
derive instance ordSqlQueryF ∷ Ord a ⇒ Ord (SqlQueryF a)

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

instance eq1SqlDeclF ∷ Eq1 SqlDeclF where
  eq1 (Import a) (Import b) = a == b
  eq1 (FunctionDecl r) (FunctionDecl rr) =
    r.ident == rr.ident
    && r.args == rr.args
    && r.body == rr.body
  eq1 _ _ = false

instance eq1SqlQueryF ∷ Eq1 SqlQueryF where
  eq1 (Query a c) (Query b d) = a == b && c == d

instance eq1SqlModuleF ∷ Eq1 SqlModuleF where
  eq1 (Module a) (Module b) = a == b

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

instance ord1SqlDeclF ∷ Ord1 SqlDeclF where
  compare1 (Import a) (Import b) = compare a b
  compare1 (Import _) _ = LT
  compare1 _ (Import _) = GT
  compare1 (FunctionDecl r) (FunctionDecl rr) =
    compare r.ident rr.ident
    <> compare r.args rr.args
    <> compare r.body rr.body

instance ord1SqlQueryF ∷ Ord1 SqlQueryF where
  compare1 (Query a c) (Query b d) = compare a b <> compare c d

instance ord1SqlModuleF ∷ Ord1 SqlModuleF where
  compare1 (Module a) (Module b) = compare a b

derive instance functorSqlF ∷ Functor l ⇒ Functor (SqlF l)
derive instance functorSqlDeclF ∷ Functor SqlDeclF
derive instance functorSqlQueryF ∷ Functor SqlQueryF
derive instance functorSqlModuleF ∷ Functor SqlModuleF

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

instance foldableSqlDeclF ∷ F.Foldable SqlDeclF where
  foldMap f = case _ of
    FunctionDecl r → f r.body
    Import _ → mempty
  foldl f a = case _ of
    FunctionDecl r → f a r.body
    Import _ → a
  foldr f a = case _ of
    FunctionDecl r → f r.body a
    Import _ → a

instance foldableSqlQueryF ∷ F.Foldable SqlQueryF where
  foldMap f (Query r s) = F.foldMap (F.foldMap f) r <> f s
  foldl f a (Query r s) = f (F.foldl (F.foldl f) a r) s
  foldr f a (Query r s) = F.foldr (\x a' → F.foldr f a' x) (f s a) r

instance foldableSqlModuleF ∷ F.Foldable SqlModuleF where
  foldMap f (Module r) = F.foldMap (F.foldMap f) r
  foldl f a (Module r) = F.foldl (F.foldl f) a r
  foldr f a (Module r) = F.foldr (\x a' → F.foldr f a' x) a r

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

instance traversableSqlDeclF ∷ T.Traversable SqlDeclF where
  traverse f = case _ of
    FunctionDecl { ident, args, body } →
      map FunctionDecl
      $ { ident, args, body: _ }
      <$> f body
    Import r → pure $ Import r
  sequence = T.sequenceDefault

instance traversableSqlQueryF ∷ T.Traversable SqlQueryF where
  traverse f (Query r s) = Query <$> T.traverse (T.traverse f) r <*> f s
  sequence = T.sequenceDefault

instance traversableSqlModuleF ∷ T.Traversable SqlModuleF where
  traverse f (Module r) = Module <$> T.traverse (T.traverse f) r
  sequence = T.sequenceDefault

printSqlF ∷ ∀ l. Algebra l String → Algebra (SqlF l) String
printSqlF printLiteralF = case _ of
  Splice Nothing →
    "*"
  Splice (Just s) →
    s <> ".*"
  SetLiteral lst →
    "(" <> F.intercalate ", " lst <> ")"
  Literal l →
    printLiteralF l
  Binop {lhs, rhs, op} →
    BO.printBinaryOperator lhs rhs op
  Unop {expr, op} →
    UO.printUnaryOperator expr op
  Ident s →
    ID.printIdent s
  InvokeFunction {name, args} →
    name <> "(" <> F.intercalate ", " args <> ")"
  Match { expr, cases, else_ } →
    "CASE "
    <> expr
    <> " "
    <> F.intercalate " " (map CS.printCase cases)
    <> F.foldMap (" ELSE " <> _) else_
    <> " END"
  Switch { cases, else_ } →
    "CASE "
    <> F.intercalate " " (map CS.printCase cases)
    <> F.foldMap (" ELSE " <> _) else_
    <> " END"
  Let { ident, bindTo, in_ } →
    ID.printIdent ident <> " := " <> bindTo <> "; " <> in_
  Vari s →
    ":" <> ID.printIdent s
  Select { isDistinct, projections, relations, filter, groupBy, orderBy } →
    "SELECT "
    <> (if isDistinct then "DISTINCT " else "")
    <> (F.intercalate ", " $ map PR.printProjection projections)
    <> (relations # F.foldMap \rs →
         " FROM " <> RL.printRelation rs)
    <> (filter # F.foldMap \f → " WHERE " <> f)
    <> (groupBy # F.foldMap \gb → " GROUP BY " <> GB.printGroupBy gb)
    <> (orderBy # F.foldMap \ob → " ORDER BY " <> OB.printOrderBy ob)
  Parens t →
    "(" <> t <> ")"

printSqlDeclF ∷ Algebra SqlDeclF String
printSqlDeclF = case _ of
  FunctionDecl { ident, args, body } →
    "CREATE FUNCTION "
    <> ID.printIdent ident
    <> "(" <> F.intercalate ", " (append ":" ∘ ID.printIdent <$> args) <> ") BEGIN "
    <> body
    <> " END"
  Import s →
    "IMPORT " <> ID.printIdent s

printSqlQueryF ∷ Algebra SqlQueryF String
printSqlQueryF (Query decls expr) = F.intercalate "; " $ L.snoc (printSqlDeclF <$> decls) expr

printSqlModuleF ∷ Algebra SqlModuleF String
printSqlModuleF (Module decls) = F.intercalate "; " $ printSqlDeclF <$> decls

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
    J.~> "cases" J.:= map CS.encodeJsonCase cases
    J.~> "else_" J.:= else_
    J.~> J.jsonEmptyObject
  Switch { cases, else_ } →
    "tag" J.:= "switch"
    J.~> "cases" J.:= map CS.encodeJsonCase cases
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
    J.~> "projections" J.:= map PR.encodeJsonProjection projections
    J.~> "relations" J.:= map RL.encodeJsonRelation relations
    J.~> "filter" J.:= filter
    J.~> "groupBy" J.:= map GB.encodeJsonGroupBy groupBy
    J.~> "orderBy" J.:= map OB.encodeJsonOrderBy orderBy
    J.~> J.jsonEmptyObject
  Parens a →
    "tag" J.:= "parens"
    J.~> "value" J.:= a
    J.~> J.jsonEmptyObject

encodeJsonSqlDeclF ∷ Algebra SqlDeclF J.Json
encodeJsonSqlDeclF = case _ of
  FunctionDecl { ident, args, body } →
    "tag" J.:= "create function"
    J.~> "ident" J.:= ident
    J.~> "args" J.:= args
    J.~> "body" J.:= body
    J.~> J.jsonEmptyObject
  Import s →
    "tag" J.:= "import"
    J.~> "value" J.:= s
    J.~> J.jsonEmptyObject

encodeJsonSqlQueryF ∷ Algebra SqlQueryF J.Json
encodeJsonSqlQueryF (Query decls expr) =
  "tag" J.:= "query"
  J.~> "decls" J.:= (encodeJsonSqlDeclF <$> decls)
  J.~> "expr" J.:= expr
  J.~> J.jsonEmptyObject

encodeJsonSqlModuleF ∷ Algebra SqlModuleF J.Json
encodeJsonSqlModuleF (Module decls) =
  "tag" J.:= "module"
  J.~> "decls" J.:= (encodeJsonSqlDeclF <$> decls)
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
    _ → E.Left $ "Invalid SQL^2 expression: " <> tag
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
    cases ← (obj J..? "cases") >>= T.traverse CS.decodeJsonCase
    else_ ← obj J..? "else_"
    pure $ Match { expr, cases, else_ }

  decodeSwitch obj = do
   cases ← (obj J..? "cases") >>= T.traverse CS.decodeJsonCase
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
    projections ← (obj J..? "projections") >>= T.traverse PR.decodeJsonProjection
    relations ← (obj J..? "relations") >>= T.traverse RL.decodeJsonRelation
    filter ← obj J..? "filter"
    groupBy ← (obj J..? "groupBy") >>= T.traverse GB.decodeJsonGroupBy
    orderBy ← (obj J..? "orderBy") >>= T.traverse OB.decodeJsonOrderBy
    pure $ Select { isDistinct, projections, relations, filter, groupBy, orderBy }

  decodeParens obj = do
    v ← obj J..? "value"
    pure $ Parens v

decodeJsonSqlDeclF ∷ CoalgebraM (E.Either String) SqlDeclF J.Json
decodeJsonSqlDeclF = J.decodeJson >=> \obj → do
  tag ← obj J..? "tag"
  case tag of
    "create function" → decodeFunctionDecl obj
    "import" → decodeImport obj
    _ → E.Left $ "Invalid SQL^2 declaration: " <> tag

  where
  decodeFunctionDecl obj = do
    ident ← obj J..? "ident"
    args ← obj J..? "args"
    body ← obj J..? "body"
    pure $ FunctionDecl { ident, args, body }

  decodeImport obj = do
    v ← obj J..? "value"
    pure $ Import v

decodeJsonSqlQueryF ∷ CoalgebraM (E.Either String) SqlQueryF J.Json
decodeJsonSqlQueryF = J.decodeJson >=> \obj → do
  tag ← obj J..? "tag"
  case tag of
    "query" → do
      decls ← T.traverse decodeJsonSqlDeclF =<< obj J..? "decls"
      expr ← obj J..? "expr"
      pure $ Query decls expr
    _ → E.Left $ "Invalid top-level SQL^2 production: " <> tag

decodeJsonSqlModuleF ∷ CoalgebraM (E.Either String) SqlModuleF J.Json
decodeJsonSqlModuleF = J.decodeJson >=> \obj → do
  tag ← obj J..? "tag"
  case tag of
    "module" → do
      decls ← T.traverse decodeJsonSqlDeclF =<< obj J..? "decls"
      pure $ Module decls
    _ → E.Left $ "Invalid top-level SQL^2 production: " <> tag

genSqlF
  ∷ ∀ m l
  . Gen.MonadGen m
  ⇒ MonadRec m
  ⇒ CoalgebraM m l Int
  → CoalgebraM m (SqlF l) Int
genSqlF genLiteral n
  | n < 2 =
  Gen.oneOf $ (Literal <$> genLiteral n) :|
    [ map Ident genIdent
    , map Vari genIdent
    , pure $ Splice Nothing
    , pure $ SetLiteral L.Nil
    ]
  | otherwise = do
  Gen.oneOf $ (Literal <$> genLiteral n) :|
    [ pure $ Splice $ Just $ n - 1
    , pure $ Parens $ n - 1
    , genSetLiteral n
    , genBinop n
    , genUnop n
    , genInvokeFunction n
    , genMatch n
    , genSwitch n
    , genLet n
    , genSelect n
    ]

genSqlDeclF ∷ ∀ m. Gen.MonadGen m ⇒ CoalgebraM m SqlDeclF Int
genSqlDeclF n =
  Gen.oneOf $ genImport :|
    [ genFunctionDecl n
    ]

genSqlQueryF ∷ ∀ m. Gen.MonadGen m ⇒ CoalgebraM m SqlQueryF Int
genSqlQueryF n = Query <$> genDecls n <*> pure n

genSqlModuleF ∷ ∀ m. Gen.MonadGen m ⇒ CoalgebraM m SqlModuleF Int
genSqlModuleF n = Module <$> genDecls n

genSetLiteral ∷ ∀ m l. Gen.MonadGen m ⇒ CoalgebraM m (SqlF l) Int
genSetLiteral n = do
  len ← Gen.chooseInt 0 $ n - 1
  pure $ SetLiteral $ map (const $ n - 1) $ L.range 0 len

genBinop ∷ ∀ m l. Gen.MonadGen m ⇒ CoalgebraM m (SqlF l) Int
genBinop n = do
  op ← BO.genBinaryOperator
  pure $ Binop { op, lhs: n - 1, rhs: n - 1 }

genUnop ∷ ∀ m l. Gen.MonadGen m ⇒ CoalgebraM m (SqlF l) Int
genUnop n = do
  op ← UO.genUnaryOperator
  pure $ Unop { op, expr: n - 1 }

genInvokeFunction ∷ ∀ m l. Gen.MonadGen m ⇒ CoalgebraM m (SqlF l) Int
genInvokeFunction n = do
  name ← genIdent
  len ← Gen.chooseInt 0 $ n - 1
  pure $ InvokeFunction { name, args: map (const $ n - 1) $ L.range 0 len }

genMatch ∷ ∀ m l. Gen.MonadGen m ⇒ CoalgebraM m (SqlF l) Int
genMatch n = do
  nothing ← Gen.chooseBool
  len ← Gen.chooseInt 0 $ n - 1
  let
    foldFn acc _ = do
      cs ← CS.genCase $ n - 1
      pure $ cs L.: acc
  cases ← L.foldM foldFn L.Nil $ L.range 0 len
  pure $ Match { expr: n - 1
               , cases
               , else_: if nothing then Nothing else Just $ n - 1
               }
genSwitch ∷ ∀ m l. Gen.MonadGen m ⇒ CoalgebraM m (SqlF l) Int
genSwitch n = do
  nothing ← Gen.chooseBool
  len ← Gen.chooseInt 0 $ n - 1
  let
    foldFn acc _ = do
      cs ← CS.genCase $ n - 1
      pure $ cs L.: acc
  cases ← L.foldM foldFn L.Nil $ L.range 0 len
  pure $ Switch { cases
                , else_: if nothing then Nothing else Just $ n - 1
                }

genLet ∷ ∀ m l. Gen.MonadGen m ⇒ CoalgebraM m (SqlF l) Int
genLet n = do
  ident ← genIdent
  pure $ Let { ident
             , bindTo: n - 1
             , in_: n - 1
             }

genSelect ∷ ∀ m l. Gen.MonadGen m ⇒ MonadRec m ⇒ CoalgebraM m (SqlF l) Int
genSelect n = do
  prjLen ← Gen.chooseInt 0 $ n - 1
  mbRelation ← Gen.chooseBool
  mbFilter ← Gen.chooseBool
  mbGroupBy ← Gen.chooseBool
  mbOrderBy ← Gen.chooseBool
  isDistinct ← Gen.chooseBool

  let
    foldPrj acc _ = do
      prj ← PR.genProjection $ n - 1
      pure $ prj L.:acc
  projections ←
    L.foldM foldPrj L.Nil $ L.range 0 prjLen

  relations ←
    if mbRelation
    then pure Nothing
    else map Just $ RL.genRelation $ n - 1

  groupBy ←
    if mbGroupBy
    then pure Nothing
    else map Just $ GB.genGroupBy $ n - 1

  orderBy ←
    if mbOrderBy
    then pure Nothing
    else map Just $ OB.genOrderBy $ n - 1

  pure $ Select { isDistinct
                , projections
                , relations
                , filter: if mbFilter then Nothing else Just $ n - 1
                , groupBy
                , orderBy
                }

genFunctionDecl ∷ ∀ m. Gen.MonadGen m ⇒ CoalgebraM m SqlDeclF Int
genFunctionDecl n = do
  ident ← genIdent
  len ← Gen.chooseInt 0 $ n - 1
  let
    foldFn acc _ = do
      arg ← genIdent
      pure $ arg L.: acc
  args ← L.foldM foldFn L.Nil $ L.range 0 len
  pure $ FunctionDecl { ident, args, body: n - 1 }

genImport ∷ ∀ m a. Gen.MonadGen m ⇒ m (SqlDeclF a)
genImport = Import <$> genIdent

genIdent ∷ ∀ m. Gen.MonadGen m ⇒ m String
genIdent = do
  start ← Gen.elements $ "a" :| S.split (S.Pattern "") "bcdefghijklmnopqrstuvwxyz"
  body ← map (Int.toStringAs Int.hexadecimal) (Gen.chooseInt 0 100000)
  pure $ start <> body

genDecls ∷ ∀ m. Gen.MonadGen m ⇒ Int → m (L.List (SqlDeclF Int))
genDecls n = do
  let
    foldFn acc _ = do
      cs ← genSqlDeclF $ n - 1
      pure $ cs L.: acc
  len ← Gen.chooseInt 0 $ n - 1
  L.foldM foldFn L.Nil $ L.range 0 len

-- This one is one gigantic TODO: generation Sql² AST that
-- can be constructed using parsing. Since parsing is
-- actually ported from quasar, this is very important
-- but annoying stuff :|

type GenSql m t = Gen.MonadGen m ⇒ MonadRec m ⇒ Corecursive t (SqlF EJ.EJsonF) ⇒ m t

genSql ∷ ∀ m t. Int → GenSql m t
genSql n
  | n < 2 = genLeaf
  | otherwise =
    Gen.oneOf $ genLetP (n - 1) :| [ genQueryExprP (n - 1) ]

genLeaf ∷ ∀ m t. GenSql m t
genLeaf =
  map (embed ∘ Literal)
  $ Gen.oneOf $ pure EJ.Null :|
    [ EJ.Boolean <$> Gen.chooseBool
    , EJ.Integer <<< HI.fromInt <$> Gen.chooseInt (-1000000) 1000000
    , EJ.Decimal <<< HN.fromNumber <$> Gen.chooseFloat (-1000000.0) 1000000.0
    , EJ.String <$> GenS.genUnicodeString
    ]

genLetP ∷ ∀ m t. Int → GenSql m t
genLetP n = do
  ident ← genIdent
  bindTo ← genSql n
  in_ ← genSql n
  pure $ embed $ Let { ident, bindTo, in_ }

genQueryExprP ∷ ∀ m t. Int → GenSql m t
genQueryExprP n
  | n < 2 = Gen.oneOf $ genQueryP n :| [ genDefinedExprP n ]
  | otherwise = do
    op ←
      Gen.elements $ BO.Limit :|
        [ BO.Offset, BO.Sample, BO.Union
        , BO.UnionAll, BO.Intersect, BO.IntersectAll
        , BO.Except
        ]
    lhs ← Gen.oneOf $ genQueryP n :| [ genDefinedExprP n ]
    rhs ← Gen.oneOf $ genQueryP n :| [ genDefinedExprP n ]
    pure $ embed $ Binop { op, lhs, rhs }

genDefinedExprP ∷ ∀ m t. Int → GenSql m t
genDefinedExprP n = do
  binops ← Gen.resize (const n) $ Gen.unfoldable BO.genBinaryOperator
  unops ← Gen.resize (const n) $ Gen.unfoldable UO.genUnaryOperator
  start ← genPrimaryExprP n
  adds ← Gen.resize (const n) $ Gen.unfoldable $ genPrimaryExprP n
  pure $ F.foldl foldFn start $ A.zip binops $ A.zip unops adds
  where
  foldFn acc (binop × unop × rhs) =
    embed
    $ Parens
    $ embed
    $ Unop
      { op: unop
      , expr: embed $ Binop { lhs: acc, rhs, op:binop }
      }

genPrimaryExprP ∷ ∀ m t. Int → GenSql m t
genPrimaryExprP n =
  Gen.oneOf $ genLeaf :|
    [ genCaseP n
    , genUnaryP n
    , genFunctionP n
    , genSetP n
    , genArrayP n
    , genMapP n
    , genSpliceP n
    , map (embed ∘ Ident) genIdent
    ]

genCaseP ∷ ∀ m t. Int → GenSql m t
genCaseP n = genLeaf

genUnaryP ∷ ∀ m t. Int → GenSql m t
genUnaryP n = genLeaf

genFunctionP ∷ ∀ m t. Int → GenSql m t
genFunctionP n = genLeaf

genSetP ∷ ∀ m t. Int → GenSql m t
genSetP n = genLeaf

genArrayP ∷ ∀ m t. Int → GenSql m t
genArrayP n = genLeaf

genMapP ∷ ∀ m t. Int → GenSql m t
genMapP n = genLeaf

genSpliceP ∷ ∀ m t. Int → GenSql m t
genSpliceP n = pure $ embed $ Splice Nothing

genQueryP ∷ ∀ m t. Int → GenSql m t
genQueryP n = genLeaf
