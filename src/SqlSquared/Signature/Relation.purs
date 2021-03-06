module SqlSquared.Signature.Relation where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either(..), either)
import Data.Foldable as F
import Data.Maybe (Maybe)
import Data.NonEmpty ((:|))
import Data.String as String
import Data.String.Gen as GenS
import Data.Traversable as T
import Matryoshka (Algebra, CoalgebraM)
import SqlSquared.Path as Pt
import SqlSquared.Signature.Ident as ID
import SqlSquared.Signature.JoinType as JT
import SqlSquared.Utils ((∘))

type JoinRelR a =
  { left ∷ Relation a
  , right ∷ Relation a
  , joinType ∷ JT.JoinType
  , clause ∷ a
  }

type ExprRelR a =
  { expr ∷ a
  , alias ∷ ID.Ident
  }

type VarRelR =
  { var ∷ ID.Ident
  , alias ∷ Maybe ID.Ident
  }

type TableRelR =
  { path ∷ Either Pt.AnyDir Pt.AnyFile
  , alias ∷ Maybe ID.Ident
  }

data Relation a
  = JoinRelation (JoinRelR a)
  | ExprRelation (ExprRelR a)
  | VarRelation VarRelR
  | TableRelation TableRelR

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
    ExprRelation  { expr, alias } →
      (ExprRelation ∘ { expr: _, alias })
      <$> f expr
    VarRelation v → pure $ VarRelation v
    TableRelation i → pure $ TableRelation i
  sequence = T.sequenceDefault

printRelation ∷ Algebra Relation String
printRelation = case _ of
  ExprRelation { expr, alias } →
    let
      indented = String.contains (String.Pattern "\n") expr
    in
      "(" <> expr <> (if indented then "\n" else "") <> ") AS " <> ID.printIdent alias
  VarRelation { var, alias } →
    ":" <> ID.printIdent var <> F.foldMap (\a → " AS " <> ID.printIdent a) alias
  TableRelation { path, alias } →
    "`"
    <> either Pt.printAnyDirPath Pt.printAnyFilePath path
    <> "`"
    <> F.foldMap (\x → " AS " <> ID.printIdent x) alias
  JoinRelation { left, right, joinType, clause } →
    printRelation left
    <> " "
    <> JT.printJoinType joinType
    <> " "
    <> printRelation right
    <> " ON "
    <> clause

genRelation ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ CoalgebraM m Relation Int
genRelation n =
  if n < 1
  then
    Gen.oneOf $ genTable :|
      [ genVar
      ]
  else
    Gen.oneOf $ genTable :|
      [ genVar
      , genJoin
      , genExpr
      ]
  where
  genVar = do
    var ← ID.Ident <$> GenS.genUnicodeString
    alias ← map ID.Ident <$> GenC.genMaybe GenS.genUnicodeString
    pure $ VarRelation { var, alias }
  genTable = do
    path ← Right <$> Pt.genAnyFilePath
    alias ← map ID.Ident <$> GenC.genMaybe GenS.genUnicodeString
    pure $ TableRelation { path, alias }
  genExpr = do
    alias ← ID.Ident <$> GenS.genUnicodeString
    pure $ ExprRelation { alias, expr: n - 1 }
  genJoin = do
    joinType ← JT.genJoinType
    left ← genRelation $ n - 1
    right ← genRelation $ n - 1
    pure $ JoinRelation { joinType, left, right, clause: n - 1 }
