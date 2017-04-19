module SqlSquare.Signature.Relation where

import Prelude

import Data.Argonaut as J
import Data.Either (Either(..), either)
import Data.Foldable as F
import Data.Int as Int
import Data.Monoid (mempty)
import Data.Traversable as T
import Data.Maybe (Maybe, maybe)
import Data.Path.Pathy as Pt

import Matryoshka (Algebra, CoalgebraM)

import SqlSquare.Signature.JoinType as JT
import SqlSquare.Signature.Ident as ID

import SqlSquare.Utils ((∘))

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

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
  -- Quasar uses unsandboxed paths because its table relation
  -- is produced by precompilation of any identifier relation
  -- that can fail if paths are incorrect. Here we provide only
  -- correct paths and omit anything → path step
  { path ∷ Either (Pt.AbsFile Pt.Sandboxed) (Pt.RelFile Pt.Sandboxed)
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
    "(" <> expr <> ") AS " <> aliasName
  VariRelation { vari, alias} →
    ":" <> ID.printIdent vari <> F.foldMap (" AS " <> _) alias
  TableRelation { path, alias } →
    "`"
    <> either Pt.printPath Pt.printPath path
    <> "`"
    <> F.foldMap (\x → " AS " <> ID.printIdent x) alias
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
  TableRelation { path, alias } →
    "tag" J.:= "table relation"
    J.~> "path" J.:= either Pt.printPath Pt.printPath path
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
    path ←
      Pt.parsePath
        (const $ Left "incorrect path")
        (const $ Left "incorrect path")

        (maybe (Left "incorrect path")
           (Right ∘ Right)
         ∘ Pt.sandbox Pt.currentDir)
        (maybe (Left "incorrect path")
           (Right ∘ Left ∘ (Pt.rootDir Pt.</> _))
         ∘ Pt.sandbox Pt.rootDir)
        pathStr
    alias ← obj J..? "alias"
    pure $ TableRelation { path, alias }

  decodeJoinRelation obj = do
    left ← decodeJsonRelation =<< obj J..? "left"
    right ← decodeJsonRelation =<< obj J..? "right"
    clause ← obj J..? "clause"
    joinType ← obj J..? "joinType"
    pure $ JoinRelation { left, right, clause, joinType }

arbitraryRelation ∷ CoalgebraM Gen.Gen Relation Int
arbitraryRelation n =
  if n < 1
  then
    Gen.oneOf genTable
      [ genVari
      ]
  else
    Gen.oneOf genTable
      [ genVari
      , genJoin
      , genExpr
      ]
  where
  genVari = do
    vari ← SC.arbitrary
    alias ← SC.arbitrary
    pure $ VariRelation { vari, alias }
  genTable = do
    let
      pathPart =
        map (Int.toStringAs Int.hexadecimal) SC.arbitrary
    dirs ← map Pt.dir <$> Gen.vectorOf n pathPart
    fileName ← map Pt.file pathPart
    let
      path = Left $ Pt.rootDir Pt.</> F.foldl (\a b → b Pt.</> a) fileName dirs
    alias ← SC.arbitrary
    pure $ TableRelation { path, alias }
  genExpr = do
    aliasName ← SC.arbitrary
    pure $ ExprRelation { aliasName, expr: n - 1 }
  genJoin = do
    joinType ← SC.arbitrary
    left ← arbitraryRelation $ n - 1
    right ← arbitraryRelation $ n - 1
    pure $ JoinRelation { joinType, left, right, clause: n - 1 }
