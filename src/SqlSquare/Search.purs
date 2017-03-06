-- | This is temp module just to be sure that it works fine

module SqlSquare.Search where

import Prelude

import Control.MonadZero (guard)

import Data.Int as Int
import Data.Lens ((.~), (?~))
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Foldable as F
import Data.Tuple (Tuple)
import Data.String as Str
import Data.String.Regex as RX
import Data.String.Regex.Unsafe as URX
import Data.String.Regex.Flags as RXF

import Global (readFloat, isNaN)

import SqlSquare.Utils ((∘), (×), (⋙))
import SqlSquare.AST (SqlF(..), Sql, FUPath, Relation(..))
import SqlSquare.AST as S
import SqlSquare.Constructors (buildSelect)

import Matryoshka (Algebra, Transform, ElgotAlgebra, cata, transAna, elgotZygo)

import Text.SlamSearch.Types as SS

stringToNumber ∷ String → Maybe Number
stringToNumber s =
  let n = readFloat s
  in if isNaN n
     then Nothing
     else Just n

stringToBoolean ∷ String → Maybe Boolean
stringToBoolean "true" = Just true
stringToBoolean "false" = Just false
stringToBoolean _ = Nothing


needDate ∷ String → Boolean
needDate = RX.test dateRegex
  where
  dateRegex =
    URX.unsafeRegex
      """^(((19|20)([2468][048]|[13579][26]|0[48])|2000)[-]02[-]29|((19|20)[0-9]{2}[-](0[4678]|1[02])[-](0[1-9]|[12][0-9]|30)|(19|20)[0-9]{2}[-](0[1359]|11)[-](0[1-9]|[12][0-9]|3[01])|(19|20)[0-9]{2}[-]02[-](0[1-9]|1[0-9]|2[0-8])))$"""
      RXF.noFlags


needTime ∷ String → Boolean
needTime = RX.test timeRegex
  where
  timeRegex =
    URX.unsafeRegex
      "^([0-1]?[0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$"
      RXF.noFlags


needDateTime ∷ String → Boolean
needDateTime = RX.test dtRegex
  where
  dtRegex =
    URX.unsafeRegex
      "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9]) (2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$"
      RXF.noFlags

needInterval ∷ String → Boolean
needInterval = RX.test intervalRegex
  where
  intervalRegex =
    URX.unsafeRegex
      "P((([0-9]*\\.?[0-9]*)Y)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)W)?(([0-9]*\\.?[0-9]*)D)?)?(T(([0-9]*\\.?[0-9]*)H)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)S)?)?"
      RXF.noFlags


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

flattenIndexF ∷ ∀ t. Transform t SqlF SqlF
flattenIndexF = case _ of
  Binop { op: S.IndexDeref, lhs } → Unop { op: S.FlattenArrayValues, expr: lhs }
  s → s

flattenIndex ∷ Sql → Sql
flattenIndex = transAna flattenIndexF

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
  | otherwise = ors $ flip predicateToSql predicate <$> L.filter (labelsPredicate labels) fields

predicateToSql ∷ Sql → SS.Predicate → Sql
predicateToSql field = case _ of
  SS.Contains (SS.Text v) →
    ors
    $ map S.pars_
    $ (pure $ S.invokeFunction_ "search"
         $ field : (S.string_ $ globToRegex $ containsToGlob v) : S.bool_ true : L.Nil
      )
    <> (sqlsFromSearchStr v <#> S.binop_ S.Eq field)
  SS.Range (SS.Text v) (SS.Text vv) →
    ors
    $ map S.pars_
    $ ( pure $ S.binop_ S.And
          ( S.pars_ $ S.binop_ S.Ge (lower_ field) (lower_ $ S.string_ v))
          ( S.pars_ $ S.binop_ S.Le (lower_ field) (lower_ $ S.string_ vv))
      )
    <> do
      start ← sqlsFromSearchStr v
      end ← sqlsFromSearchStr vv
      pure $ S.binop_ S.And
        ( S.pars_ $ S.binop_ S.Ge field start )
        ( S.pars_ $ S.binop_ S.Le field end )
  SS.Range (SS.Tag val) vv →
    predicateToSql field $ SS.Range (SS.Text val) vv
  SS.Range val (SS.Tag vv) →
    predicateToSql field $ SS.Range val (SS.Text vv)
  SS.Contains (SS.Tag v) →
    predicateToSql field $ SS.Contains $ SS.Text v

  SS.Eq v → renderBinRel S.Eq $ valueToString v
  SS.Gt v → renderBinRel S.Gt $ valueToString v
  SS.Gte v → renderBinRel S.Ge $ valueToString v
  SS.Lt v → renderBinRel S.Lt $ valueToString v
  SS.Lte v → renderBinRel S.Le $ valueToString v
  SS.Ne v → renderBinRel S.Neq $ valueToString v
  SS.Like v →
    S.invokeFunction_ "search"
      $ field : S.string_ v : S.bool_ true : L.Nil
  where
  valueToString ∷ SS.Value → String
  valueToString = case _ of
    SS.Text v → v
    SS.Tag v → v

  renderBinRel ∷ S.BinaryOperator → String → Sql
  renderBinRel op v =
    ors
    $ map S.pars_
    ( pure $ S.binop_ op (lower_ field) (lower_ $ S.string_ v))
    <> ( sqlsFromSearchStr v <#> S.binop_ op field)


  sqlsFromSearchStr ∷ String → L.List Sql
  sqlsFromSearchStr v =
    (flip F.foldMap (stringToNumber v) $ pure ∘ S.num_)
    <> (flip F.foldMap (Int.fromString v) $ pure ∘ S.int_)
    <> (flip F.foldMap (stringToBoolean v) $ pure ∘ S.bool_)
    <> ((guard ((not $ needDateTime v) && needDate v)) $>
        S.invokeFunction_ "DATE" (S.string_ v : L.Nil))
    <> (guard (needTime v) $>
        S.invokeFunction_ "TIME" (S.string_ v : L.Nil))
    <> (guard (needDateTime v) $>
        S.invokeFunction_ "TIMESTAMP" (S.string_ v : L.Nil))
    <> (guard (needInterval v) $>
        S.invokeFunction_ "INTERVAL" (S.string_ v : L.Nil))

  lower_ ∷ Sql → Sql
  lower_ = S.invokeFunction_ "LOWER" ∘ pure

globToRegex ∷ String → String
globToRegex =
  (\x → "^" <> x <> "$")
    ∘ RX.replace askRegex "."
    ∘ RX.replace starRegex ".*"
    ∘ RX.replace globEscapeRegex "\\$&"
  where
    globEscapeRegex =
      URX.unsafeRegex
        "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|]"
        RXF.global

    starRegex =
      URX.unsafeRegex
        "\\*" RXF.global
    askRegex =
      URX.unsafeRegex
        "\\?" RXF.global

containsToGlob ∷ String → String
containsToGlob v
  | hasSpecialChars v = v
  | otherwise = "*" <> v <> "*"

hasSpecialChars ∷ String → Boolean
hasSpecialChars v =
  isJust (Str.indexOf (Str.Pattern "*") v) || isJust (Str.indexOf (Str.Pattern "?") v)

listIndexF ∷ Algebra SqlF (Maybe Int)
listIndexF = case _ of
  Splice Nothing → Just 0
  Splice (Just i) → map (add one) i
  Binop { op: S.FieldDeref, lhs: Just i } → Just $ i + one
  Binop { op: S.IndexDeref, lhs: Just i } → Just $ i + one
  Unop { op: S.FlattenArrayValues, expr: Just i } → Just $ i + one
  Unop { op: S.FlattenMapValues, expr: Just i } → Just $ i + one
  _ → Nothing


identOrString ∷ ∀ a. SqlF a → Maybe String
identOrString (Ident s) = Just s
identOrString (StringLiteral s) = Just s
identOrString _ = Nothing

labelPredicateF ∷ L.List String → ElgotAlgebra (Tuple (Maybe Int)) SqlF Boolean
labelPredicateF labelsString (mbIx × sqlF) = case sqlF of
  Splice acc →
    fromMaybe false acc && ixedLabel == Just "*"
  IntLiteral i →
    fromMaybe false $ ixedLabel >>= Int.fromString ⋙ map (eq i)
  StringLiteral i →
    ixedLabel == Just i
  Ident i →
    ixedLabel == Just i
  Unop {op: S.FlattenArrayValues, expr} →
    expr && (ixedLabel == Just "*" || ixedLabel == Just "[*]")
  Unop {op: S.FlattenMapValues, expr} →
    expr && (ixedLabel == Just "*" || ixedLabel == Just "{*}")
  Binop { op: S.FieldDeref, lhs, rhs } →
    lhs && rhs
  Binop { op: S.IndexDeref, lhs, rhs } →
    lhs && rhs
  _ →
    false
  where
  ixedLabel ∷ Maybe String
  ixedLabel = mbIx >>= L.index labelsString

labelsPredicate ∷ L.List SS.Label → Sql → Boolean
labelsPredicate ls = elgotZygo listIndexF (labelPredicateF $ labelStrings ls )


labelStrings ∷ ∀ f. Functor f ⇒ f SS.Label → f String
labelStrings = map case _ of
  SS.Meta l → l
  SS.Common l → l
