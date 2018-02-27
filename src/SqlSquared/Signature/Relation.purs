module SqlSquared.Signature.Relation where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut as J
import Data.Either (Either(..))
import Data.Foldable as F
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Data.NonEmpty ((:|))
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
  , aliasName ∷ String
  }

type VariRelR =
  { vari ∷ String
  , alias ∷ Maybe String
  }

type TableRelR =
  { path ∷ Pt.AnyFile
  , alias ∷ Maybe String
  }

data Relation a
  = JoinRelation (JoinRelR a)
  | ExprRelation (ExprRelR a)
  | VariRelation VariRelR
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
    ExprRelation  { expr, aliasName} →
      (ExprRelation ∘ { expr: _, aliasName})
      <$> f expr
    VariRelation v → pure $ VariRelation v
    TableRelation i → pure $ TableRelation i
  sequence = T.sequenceDefault

printRelation ∷ Algebra Relation String
printRelation = case _ of
  ExprRelation {expr, aliasName} →
    "(" <> expr <> ") AS " <> ID.printIdent aliasName
  VariRelation { vari, alias} →
    ":" <> ID.printIdent vari <> F.foldMap (\a → " AS " <> ID.printIdent a) alias
  TableRelation { path, alias } →
    "`"
    <> Pt.printAnyFilePath path
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
  TableRelation { path, alias } →
    "tag" J.:= "table relation"
    J.~> "path" J.:= Pt.printAnyFilePath path
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
    pathStr ← obj J..? "path"
    path ← Pt.parseAnyFilePath Left pathStr
    alias ← obj J..? "alias"
    pure $ TableRelation { path, alias }

  decodeJoinRelation obj = do
    left ← decodeJsonRelation =<< obj J..? "left"
    right ← decodeJsonRelation =<< obj J..? "right"
    clause ← obj J..? "clause"
    joinType ← obj J..? "joinType"
    pure $ JoinRelation { left, right, clause, joinType }

genRelation ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ CoalgebraM m Relation Int
genRelation n =
  if n < 1
  then
    Gen.oneOf $ genTable :|
      [ genVari
      ]
  else
    Gen.oneOf $ genTable :|
      [ genVari
      , genJoin
      , genExpr
      ]
  where
  genVari = do
    vari ← GenS.genUnicodeString
    alias ← GenC.genMaybe GenS.genUnicodeString
    pure $ VariRelation { vari, alias }
  genTable = do
    path ← Pt.genAnyFilePath
    alias ← GenC.genMaybe GenS.genUnicodeString
    pure $ TableRelation { path, alias }
  genExpr = do
    aliasName ← GenS.genUnicodeString
    pure $ ExprRelation { aliasName, expr: n - 1 }
  genJoin = do
    joinType ← JT.genJoinType
    left ← genRelation $ n - 1
    right ← genRelation $ n - 1
    pure $ JoinRelation { joinType, left, right, clause: n - 1 }
