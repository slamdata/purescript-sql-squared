module SqlSquare.Relation where

import Prelude

import Data.Either (Either, either)
import Data.Foldable as F
import Data.Monoid (mempty)
import Data.Traversable as T
import Data.Maybe (Maybe)
import Data.Path.Pathy (AbsFile, RelFile, Unsandboxed, unsafePrintPath)

import Matryoshka (Algebra)

import SqlSquare.JoinType as JT
import SqlSquare.Utils ((∘))

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
