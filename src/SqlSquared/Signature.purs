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
import Data.Array as A
import Data.Eq (class Eq1)
import Data.Foldable as F
import Data.HugeInt as HI
import Data.HugeNum as HN
import Data.Int as Int
import Data.Json.Extended as EJ
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Ord (class Ord1)
import Data.String as S
import Data.String.Gen as GenS
import Data.Traversable as T
import Matryoshka (Algebra, CoalgebraM, class Corecursive, embed)
import SqlSquared.Path as Pt
import SqlSquared.Signature.BinaryOperator (BinaryOperator(..), binopFromString, binopToString, genBinaryOperator, printBinaryOperator) as BO
import SqlSquared.Signature.Case (Case(..), genCase, printCase) as CS
import SqlSquared.Signature.GroupBy (GroupBy(..), genGroupBy, printGroupBy) as GB
import SqlSquared.Signature.Ident (Ident(..), printIdent) as ID
import SqlSquared.Signature.JoinType (JoinType(..), genJoinType, joinTypeFromString, printJoinType) as JT
import SqlSquared.Signature.OrderBy (OrderBy(..), genOrderBy, printOrderBy) as OB
import SqlSquared.Signature.OrderType (OrderType(..), genOrderType, orderTypeFromString, printOrderType) as OT
import SqlSquared.Signature.Projection (Projection(..), genProjection, printProjection) as PR
import SqlSquared.Signature.Relation (ExprRelR, JoinRelR, Relation(..), TableRelR, VarRelR, genRelation, printRelation) as RL
import SqlSquared.Signature.UnaryOperator (UnaryOperator(..), genUnaryOperator, printUnaryOperator, unopFromString, unopToString) as UO
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
  { name ∷ ID.Ident
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
  { ident ∷ ID.Ident
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
  { ident ∷ ID.Ident
  , args ∷ L.List ID.Ident
  , body ∷ a
  }

data SqlF literal a
  = SetLiteral (L.List a)
  | Literal (literal a)
  | Splice (Maybe a)
  | Binop (BinopR a)
  | Unop (UnopR a)
  | Identifier ID.Ident
  | InvokeFunction (InvokeFunctionR a)
  | Match (MatchR a)
  | Switch (SwitchR a)
  | Let (LetR a)
  | Var ID.Ident
  | Select (SelectR a)
  | Parens a

data SqlDeclF a
  = Import Pt.AnyDir
  | FunctionDecl (FunctionDeclR a)

newtype SqlModuleF a =
  Module (L.List (SqlDeclF a))

data SqlQueryF a =
  Query (L.List (SqlDeclF a)) a

derive instance eqSqlF ∷ (Eq a, Eq1 l) ⇒ Eq (SqlF l a)
derive instance ordSlqF ∷ (Ord a, Ord1 l) ⇒ Ord (SqlF l a)
derive instance eq1SqlF ∷ Eq1 l ⇒ Eq1 (SqlF l)
derive instance ord1SqlF ∷ Ord1 l ⇒ Ord1 (SqlF l)

derive instance eqSqlDeclF ∷ Eq a ⇒ Eq (SqlDeclF a)
derive instance ordSqlDeclF ∷ Ord a ⇒ Ord (SqlDeclF a)
derive instance eq1SqlDeclF ∷ Eq1 SqlDeclF
derive instance ord1SqlDeclF ∷ Ord1 SqlDeclF

derive instance eqSqlModuleF ∷ Eq a ⇒ Eq (SqlModuleF a)
derive instance ordSqlModuleF ∷ Ord a ⇒ Ord (SqlModuleF a)
derive instance newtypeSqlModuleF ∷ Newtype (SqlModuleF a) _
derive instance eq1SqlModuleF ∷ Eq1 SqlModuleF
derive instance ord1SqlModuleF ∷ Ord1 SqlModuleF

derive instance eqSqlQueryF ∷ Eq a ⇒ Eq (SqlQueryF a)
derive instance ordSqlQueryF ∷ Ord a ⇒ Ord (SqlQueryF a)
derive instance eq1SqlQueryF ∷ Eq1 SqlQueryF
derive instance ord1SqlQueryF ∷ Ord1 SqlQueryF

derive instance functorSqlF ∷ Functor l ⇒ Functor (SqlF l)
derive instance functorSqlDeclF ∷ Functor SqlDeclF
derive instance functorSqlQueryF ∷ Functor SqlQueryF
derive instance functorSqlModuleF ∷ Functor SqlModuleF

instance foldableSqlF ∷ F.Foldable l ⇒ F.Foldable (SqlF l) where
  foldMap f = case _ of
    Identifier _ → mempty
    SetLiteral lst → F.foldMap f lst
    Splice mbA → F.foldMap f mbA
    Binop { lhs, rhs } → f lhs <> f rhs
    Unop { expr } → f expr
    InvokeFunction { args } → F.foldMap f args
    Match { expr, cases, else_ } → f expr <> F.foldMap (F.foldMap f) cases <> F.foldMap f else_
    Switch { cases, else_} → F.foldMap (F.foldMap f) cases <> F.foldMap f else_
    Let { bindTo, in_ } → f bindTo <> f in_
    Var _ → mempty
    Select { projections, relations, filter, groupBy, orderBy } →
      F.foldMap (F.foldMap f) projections
      <> F.foldMap (F.foldMap f) relations
      <> F.foldMap f filter
      <> F.foldMap (F.foldMap f) groupBy
      <> F.foldMap (F.foldMap f) orderBy
    Parens a → f a
    Literal l → F.foldMap f l
  foldl f a = case _ of
    Identifier _ → a
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
    Var _ → a
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
    Identifier _ → a
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
    Var _ → a
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
    Identifier s → pure $ Identifier s
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
    Var s → pure $ Var s
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
  Identifier s →
    ID.printIdent s
  InvokeFunction {name, args} →
    ID.printIdent name <> "(" <> F.intercalate ", " args <> ")"
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
  Var s →
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
  Import path →
    "IMPORT " <> ID.printIdent (ID.Ident (Pt.printAnyDirPath path))

printSqlQueryF ∷ Algebra SqlQueryF String
printSqlQueryF (Query decls expr) = F.intercalate "; " $ L.snoc (printSqlDeclF <$> decls) expr

printSqlModuleF ∷ Algebra SqlModuleF String
printSqlModuleF (Module decls) = F.intercalate "; " $ printSqlDeclF <$> decls

genSqlF
  ∷ ∀ m l
  . Gen.MonadGen m
  ⇒ MonadRec m
  ⇒ CoalgebraM m l Int
  → CoalgebraM m (SqlF l) Int
genSqlF genLiteral n
  | n < 2 =
  Gen.oneOf $ (Literal <$> genLiteral n) :|
    [ map Identifier genIdent
    , map Var genIdent
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

genSqlDeclF ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ CoalgebraM m SqlDeclF Int
genSqlDeclF n =
  Gen.oneOf $ genImport :|
    [ genFunctionDecl n
    ]

genSqlQueryF ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ CoalgebraM m SqlQueryF Int
genSqlQueryF n = Query <$> genDecls n <*> pure n

genSqlModuleF ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ CoalgebraM m SqlModuleF Int
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

genImport ∷ ∀ m a. Gen.MonadGen m ⇒ MonadRec m ⇒ m (SqlDeclF a)
genImport = map Import Pt.genAnyDirPath

genIdent ∷ ∀ m. Gen.MonadGen m ⇒ m ID.Ident
genIdent = do
  start ← Gen.elements $ "a" :| S.split (S.Pattern "") "bcdefghijklmnopqrstuvwxyz"
  body ← map (Int.toStringAs Int.hexadecimal) (Gen.chooseInt 0 100000)
  pure $ ID.Ident (start <> body)

genDecls ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ Int → m (L.List (SqlDeclF Int))
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
    , map (embed ∘ Identifier) genIdent
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
