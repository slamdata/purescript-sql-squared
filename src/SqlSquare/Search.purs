-- | This is temp module just to be sure that it works fine

module SqlSquare.Search where

import Prelude

import Data.Lens ((.~), (?~))
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (unwrap)
import Data.Foldable as F

import SqlSquare.Utils ((∘), type (×), (×), (⋙))
import SqlSquare.AST (SqlF(..), Sql, buildSelect, FUPath, SqlRelation(..))
import SqlSquare.AST as S

import Matryoshka (cata, Algebra, project, cata)

import Text.SlamSearch.Types as SS

data TopFieldMark
  = Init
  | Uno
  | Duo

isTop ∷ TopFieldMark → Boolean
isTop Duo = false
isTop _ = true

topFieldF ∷ Algebra SqlF TopFieldMark
topFieldF = case _ of
  Splice Nothing → Init
  StringLiteral _ → Uno
  Ident _ → Uno
  IntLiteral _ → Uno
  Binop { op: S.FieldDeref, lhs: Init, rhs: Uno } → Uno
  Binop { op: S.IndexDeref, lhs: Init, rhs: Uno } → Uno
  _ → Duo

topField ∷ Sql → Boolean
topField = isTop ∘ cata topFieldF

queryToSql
  ∷ L.List Sql
  → SS.SearchQuery
  → FUPath
  → Sql
queryToSql fields query tablePath =
  buildSelect
    $ (S._isDistinct .~ isDistinct)
    ∘ (S._projections .~ topFields)
    ∘ (S._relations ?~ TableRelation {alias: Nothing, tablePath})
    ∘ (S._filter ?~ filter)

  where
  topFields = map (S.Projection ∘ { expr: _, alias: Nothing }) $ L.filter topField fields

  isDistinct = false

  filter =
    ands
    $ map ors
    $ unwrap
    $ map (termToSql fields) query

ors ∷ L.List Sql → Sql
ors = case _ of
  L.Nil → S.bool_ false
  hd : L.Nil → S.pars_ hd
  hd : tl → F.foldl (\acc sql → S.binop_ S.Or acc $ S.pars_ sql) hd tl

ands ∷ L.List Sql → Sql
ands = case _ of
  L.Nil → S.bool_ true
  hd : L.Nil → S.pars_ hd
  hd : tl → F.foldl (\acc sql → S.binop_ S.And acc $ S.pars_ sql) hd tl

termToSql ∷ L.List Sql → SS.Term → Sql
termToSql fields (SS.Term {include, predicate, labels})
  | not include =
      S.unop_ S.Not $ S.pars_ $ termToSql fields (SS.Term {include: true, predicate, labels})
  | otherwise = S.bool_ false --ors $ map (predicateToSql predicate) $ labelsProjection fields labels


--labelsProjection
--  ∷ L.List Sql
--  → Array SS.Label
--  → L.List Sql
--labelsProjection fields ls =
