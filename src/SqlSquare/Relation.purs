module SqlSquare.Relation where

import Prelude

import Data.Argonaut as J
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Int as Int
import Data.Foldable as F
import Data.Monoid (mempty)
import Data.Traversable as T
import Data.Maybe (Maybe)
import Data.Path.Pathy (AbsFile, RelFile, Unsandboxed, unsafePrintPath, parsePath, (</>))
import Data.Path.Pathy as Pt

import Matryoshka (Algebra, CoalgebraM)

import SqlSquare.JoinType as JT
import SqlSquare.Utils ((∘))

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

type FUPath = Either (RelFile Unsandboxed) (AbsFile Unsandboxed)

type JoinRelR a =
  { left ∷ Relation a
  , right ∷ Relation a
  , joinType ∷ JT.JoinType
  , clause ∷ a
  }

type ExprRelR a =
  { expr ∷ a
  , aliasName ∷ String
  }

type TableRelR a =
  { tablePath ∷ FUPath
  , alias ∷ Maybe String
  }

type VariRelR a =
  { vari ∷ String
  , alias ∷ Maybe String
  }

type IdentRelR =
  { ident ∷ String
  , alias ∷ Maybe String
  }

data Relation a
  = JoinRelation (JoinRelR a)
  | ExprRelation (ExprRelR a)
  | TableRelation (TableRelR a)
  | VariRelation (VariRelR a)
  | IdentRelation IdentRelR

derive instance functorRelation ∷ Functor Relation
derive instance eqRelation ∷ Eq a ⇒ Eq (Relation a)
derive instance ordRelation ∷ Ord a ⇒ Ord (Relation a)
instance foldableRelation ∷ F.Foldable Relation where
  foldMap f = case _ of
    JoinRelation { left, right, clause } → F.foldMap f left <> F.foldMap f right <> f clause
    ExprRelation { expr } → f expr
    _ → mempty
  foldl f a = case _ of
    JoinRelation { left, right, clause } →
      f (F.foldl f (F.foldl f a left) right) clause
    ExprRelation { expr } →
      f a expr
    _ → a
  foldr f a = case _ of
    JoinRelation { left, right, clause } →
      F.foldr f (F.foldr f (f clause a) right) left
    ExprRelation { expr } →
      f expr a
    _ → a
instance traversableRelation ∷ T.Traversable Relation where
  traverse f = case _ of
    JoinRelation { left, right, clause, joinType } →
      map JoinRelation
      $ { joinType, left: _, right: _, clause: _}
      <$> T.traverse f left
      <*> T.traverse f right
      <*> f clause
    ExprRelation  { expr, aliasName} →
      (ExprRelation ∘ { expr: _, aliasName})
      <$> f expr
    TableRelation t → pure $ TableRelation t
    VariRelation v → pure $ VariRelation v
    IdentRelation i → pure $ IdentRelation i
  sequence = T.sequenceDefault

printRelation ∷ Algebra Relation String
printRelation = case _ of
  ExprRelation {expr, aliasName} →
    "(" <> expr <> ") AS " <> aliasName
  VariRelation { vari, alias} →
    vari <> F.foldMap (" AS " <> _) alias
  TableRelation { tablePath, alias } →
    "`"
    <> either unsafePrintPath unsafePrintPath tablePath
    <> "`"
    <> F.foldMap (" AS " <> _) alias
  IdentRelation { ident, alias } →
    ident <> F.foldMap (\x → " AS `" <> x <> "`") alias
  JoinRelation { left, right, joinType, clause } →
    printRelation left
    <> " "
    <> JT.printJoinType joinType
    <> " "
    <> printRelation right
    <> " on "
    <> clause

encodeJsonRelation ∷ Algebra Relation J.Json
encodeJsonRelation = case _ of
  ExprRelation { expr, aliasName } →
    "tag" J.:= "expr relation"
    J.~> "expr" J.:= expr
    J.~> "aliasName" J.:= aliasName
    J.~> J.jsonEmptyObject
  VariRelation { vari, alias } →
    "tag" J.:= "vari relation"
    J.~> "vari" J.:= vari
    J.~> "alias" J.:= alias
    J.~> J.jsonEmptyObject
  TableRelation { tablePath, alias } →
    "tag" J.:= "table relation"
    J.~> "tablePath" J.:= either unsafePrintPath unsafePrintPath tablePath
    J.~> "alias" J.:= alias
    J.~> J.jsonEmptyObject
  IdentRelation { ident, alias } →
    "tag" J.:= "ident relation"
    J.~> "ident" J.:= ident
    J.~> "alias" J.:= alias
    J.~> J.jsonEmptyObject
  JoinRelation { left, right, joinType, clause } →
    "tag" J.:= "join relation"
    J.~> "left" J.:= encodeJsonRelation left
    J.~> "right" J.:= encodeJsonRelation right
    J.~> "joinType" J.:= joinType
    J.~> "clause" J.:= clause
    J.~> J.jsonEmptyObject

decodeJsonRelation ∷ CoalgebraM (Either String) Relation J.Json
decodeJsonRelation = J.decodeJson >=> \obj → do
  tag ← obj J..? "tag"
  case tag of
    "expr relation" → decodeExprRelation obj
    "vari relation" → decodeVariRelation obj
    "table relation" → decodeTableRelation obj
    "ident relation" → decodeIdentRelation obj
    "join relation" → decodeJoinRelation obj
    _ → Left "This is not join relation"
  where
  decodeExprRelation obj = do
    expr ← obj J..? "expr"
    aliasName ← obj J..? "aliasName"
    pure $ ExprRelation { expr, aliasName }

  decodeVariRelation obj = do
    vari ← obj J..? "vari"
    alias ← obj J..? "alias"
    pure $ VariRelation { vari, alias }


  decodeTableRelation obj = do
    tableString ← obj J..? "tablePath"
    alias ← obj J..? "alias"
    tablePath ←
      parsePath
        (const $ Left "Incorrect table path in table relation")
        (const $ Left "Incorrect table path in table relation")
        (Right ∘ Left)
        (Right ∘ Right)
        tableString
    pure $ TableRelation { tablePath, alias }

  decodeIdentRelation obj = do
    ident ← obj J..? "ident"
    alias ← obj J..? "alias"
    pure $ IdentRelation { ident, alias }


  decodeJoinRelation obj = do
    left ← decodeJsonRelation =<< obj J..? "left"
    right ← decodeJsonRelation =<< obj J..? "right"
    clause ← obj J..? "clause"
    joinType ← obj J..? "joinType"
    pure $ JoinRelation { left, right, clause, joinType }

genPath ∷ Int → Gen.Gen FUPath
genPath n = do
  d ← A.foldM foldFn Pt.rootDir $ A.replicate n unit
  name ← map (Int.toStringAs Int.hexadecimal) SC.arbitrary
  pure $ Right $ d </> Pt.file name
  where
  foldFn pt _ = do
    name ← map (Int.toStringAs Int.hexadecimal) SC.arbitrary
    pure $ pt </> Pt.dir name


arbitraryRelation ∷ CoalgebraM Gen.Gen Relation Int
arbitraryRelation n =
  if n < 1
  then
    Gen.oneOf genTable
      [ genVari
      , genIdent
      ]
  else
    Gen.oneOf genTable
      [ genVari
      , genIdent
      , genJoin
      , genExpr
      ]
  where
  genVari = do
    vari ← SC.arbitrary
    alias ← SC.arbitrary
    pure $ VariRelation { vari, alias }
  genIdent = do
    ident ← SC.arbitrary
    alias ← SC.arbitrary
    pure $ IdentRelation { ident, alias }
  genTable = do
    tablePath ← genPath $ n + 2
    alias ← SC.arbitrary
    pure $ TableRelation { tablePath, alias }
  genExpr = do
    aliasName ← SC.arbitrary
    pure $ ExprRelation { aliasName, expr: n - 1 }
  genJoin = do
    joinType ← SC.arbitrary
    left ← arbitraryRelation $ n - 1
    right ← arbitraryRelation $ n - 1
    pure $ JoinRelation { joinType, left, right, clause: n - 1 }
