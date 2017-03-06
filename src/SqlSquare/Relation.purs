module SqlSquare.Relation where

import Prelude

import Data.Either (Either, either)
import Data.Foldable as F
import Data.Maybe (Maybe)
import Data.Path.Pathy (AbsFile, RelFile, Unsandboxed, unsafePrintPath)

import Matryoshka (Algebra)

import SqlSquare.JoinType as JT

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

printRelation ∷ Algebra Relation String
printRelation = case _ of
  ExprRelation {expr, aliasName} →
    "(" <> expr <> ") as " <> aliasName
  VariRelation { vari, alias} →
    vari <> F.foldMap (" as " <> _) alias
  TableRelation { tablePath, alias } →
    "`"
    <> either unsafePrintPath unsafePrintPath tablePath
    <> "`"
    <> F.foldMap (" as " <> _) alias
  IdentRelation { ident, alias } →
    ident <> F.foldMap (" as " <> _) alias
  JoinRelation { left, right, joinType, clause } →
    printRelation left
    <> " "
    <> JT.printJoinType joinType
    <> " "
    <> printRelation right
    <> " on "
    <> clause
