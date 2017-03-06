-- | In this example `purescript-search` query is interpreted to Sql²
-- | using additional `List Sql` with projections (see `Test.Argonaut` to find out how to
-- | get it)
module Test.Search where

import Prelude

import Control.MonadZero (guard)

import Data.Foldable as F
import Data.Int as Int
import Data.Lens ((.~), (?~))
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX
import Data.Tuple (Tuple)

import Global (readFloat, isNaN)

import SqlSquare as S
import SqlSquare.Utils ((∘), (×), (⋙))

import Matryoshka (Algebra, Transform, ElgotAlgebra, cata, transAna, elgotZygo)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert

import Text.SlamSearch.Types as SS

--------------------------------------------------------------------------------
-- Guards and filters
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Accessors
--------------------------------------------------------------------------------

labelStrings ∷ ∀ f. Functor f ⇒ f SS.Label → f String
labelStrings = map case _ of
  SS.Meta l → l
  SS.Common l → l

identOrString ∷ ∀ a. S.SqlF a → Maybe String
identOrString = case _ of
  S.Ident s → Just s
  S.StringLiteral s → Just s
  _ → Nothing

valueToString ∷ SS.Value → String
valueToString = case _ of
  SS.Text v → v
  SS.Tag v → v

--------------------------------------------------------------------------------
-- Predicate aggregations
--------------------------------------------------------------------------------

ors ∷ L.List S.Sql → S.Sql
ors = case _ of
  L.Nil → S.bool false
  hd : L.Nil → S.pars hd
  hd : tl → F.foldl (\acc sql → S.binop S.Or acc $ S.pars sql) hd tl

ands ∷ L.List S.Sql → S.Sql
ands = case _ of
  L.Nil → S.bool true
  hd : L.Nil → S.pars hd
  hd : tl → F.foldl (\acc sql → S.binop S.And acc $ S.pars sql) hd tl

--------------------------------------------------------------------------------
-- Filtering only top fields
--------------------------------------------------------------------------------

data TopFieldMark
  = Init
  | TopField
  | NotTopField

isTop ∷ TopFieldMark → Boolean
isTop = case _ of
  NotTopField → false
  _ → true

topFieldF ∷ Algebra S.SqlF TopFieldMark
topFieldF = case _ of
  S.Splice Nothing → Init
  S.StringLiteral _ → TopField
  S.Ident _ → TopField
  S.IntLiteral _ → TopField
  S.Binop { op: S.FieldDeref, lhs: Init, rhs: TopField } → TopField
  S.Binop { op: S.IndexDeref, lhs: Init, rhs: TopField } → TopField
  _ → NotTopField

isTopField ∷ S.Sql → Boolean
isTopField = isTop ∘ cata topFieldF

--------------------------------------------------------------------------------
-- Flattening all array derefs ( `foo[1]` → `foo[*]` )
--------------------------------------------------------------------------------

flattenIndexF ∷ ∀ t. Transform t S.SqlF S.SqlF
flattenIndexF = case _ of
  S.Binop { op: S.IndexDeref, lhs } → S.Unop { op: S.FlattenArrayValues, expr: lhs }
  s → s

flattenIndex ∷ S.Sql → S.Sql
flattenIndex = transAna flattenIndexF

--------------------------------------------------------------------------------
-- Searching for flatten values ({*}, [*])
--------------------------------------------------------------------------------

needDistinctF ∷ Algebra S.SqlF Boolean
needDistinctF = case _ of
  S.Unop { op: S.FlattenArrayValues } → true
  S.Unop { op: S.FlattenMapValues } → true
  S.Binop { lhs, rhs } → lhs || rhs
  _ → false

needDistinct ∷ S.Sql → Boolean
needDistinct = cata needDistinctF

--------------------------------------------------------------------------------
-- Interpretation
--------------------------------------------------------------------------------

termToSql ∷ L.List S.Sql → SS.Term → S.Sql
termToSql fields (SS.Term { include, predicate, labels})
  | not include =
      S.unop S.Not $ S.pars $ termToSql fields $ SS.Term { include: true, predicate, labels}
  | otherwise =
      ors $ flip predicateToSql predicate <$> L.filter (labelsConform labels) fields

-- | This function checks if Sql field is conforming search label list
labelsConform ∷ L.List SS.Label → S.Sql → Boolean
labelsConform ls = elgotZygo listIndexF $ labelsConformF $ labelStrings ls

-- | Algebra of deref depth
listIndexF ∷ Algebra S.SqlF (Maybe Int)
listIndexF = case _ of
  S.Splice Nothing → Just 0
  S.Splice (Just i) → map (add one) i
  S.Binop { op: S.FieldDeref, lhs: Just i } → Just $ i + one
  S.Binop { op: S.IndexDeref, lhs: Just i } → Just $ i + one
  S.Unop { op: S.FlattenArrayValues, expr: Just i } → Just $ i + one
  S.Unop { op: S.FlattenMapValues, expr: Just i } → Just $ i + one
  _ → Nothing

-- | Algebra checking that deref rhs conforms label from label list
labelsConformF ∷ L.List String → ElgotAlgebra (Tuple (Maybe Int)) S.SqlF Boolean
labelsConformF labelsString (mbIx × sqlF) = case sqlF of
  S.Splice acc →
    fromMaybe false acc && ixedLabel == Just "*"
  S.IntLiteral i →
    fromMaybe false $ ixedLabel >>= Int.fromString ⋙ map (eq i)
  S.StringLiteral i →
    ixedLabel == Just i
  S.Ident i →
    ixedLabel == Just i
  S.Unop {op: S.FlattenArrayValues, expr} →
    expr && (ixedLabel == Just "*" || ixedLabel == Just "[*]")
  S.Unop {op: S.FlattenMapValues, expr} →
    expr && (ixedLabel == Just "*" || ixedLabel == Just "{*}")
  S.Binop { op: S.FieldDeref, lhs, rhs } →
    lhs && rhs
  S.Binop { op: S.IndexDeref, lhs, rhs } →
    lhs && rhs
  _ →
    false
  where
  ixedLabel ∷ Maybe String
  ixedLabel = mbIx >>= L.index labelsString


-- | Getting sql field and search predicate construct sql predicate
predicateToSql ∷ S.Sql → SS.Predicate → S.Sql
predicateToSql field = case _ of
  SS.Contains (SS.Text v) →
    ors
    $ (pure
         $ S.invokeFunction "search"
         $ field
         : (S.string $ globToRegex $ containsToGlob v)
         : S.bool true
         : L.Nil
      )
    <> (sqlsFromSearchStr v <#> S.binop S.Eq field)
  SS.Range (SS.Text v) (SS.Text vv) →
    ors
    $ ( pure $ S.binop S.And
          ( S.pars $ S.binop S.Ge (lower field) (lower $ S.string v))
          ( S.pars $ S.binop S.Le (lower field) (lower $ S.string vv))
      )
    <> do
      start ← sqlsFromSearchStr v
      end ← sqlsFromSearchStr vv
      pure $ S.binop S.And
        ( S.pars $ S.binop S.Ge field start )
        ( S.pars $ S.binop S.Le field end )
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
    S.invokeFunction "search"
      $ field : S.string v : S.bool true : L.Nil
  where
  renderBinRel ∷ S.BinaryOperator → String → S.Sql
  renderBinRel op v =
    ors
    $ ( pure $ S.binop op (lower field) (lower $ S.string v))
    <> ( sqlsFromSearchStr v <#> S.binop op field)

  sqlsFromSearchStr ∷ String → L.List S.Sql
  sqlsFromSearchStr v =
    (flip F.foldMap (stringToNumber v) $ pure ∘ S.num)
    <> (flip F.foldMap (Int.fromString v) $ pure ∘ S.int)
    <> (flip F.foldMap (stringToBoolean v) $ pure ∘ S.bool)
    <> ((guard ((not $ needDateTime v) && needDate v)) $>
        S.invokeFunction "DATE" (S.string v : L.Nil))
    <> (guard (needTime v) $>
        S.invokeFunction "TIME" (S.string v : L.Nil))
    <> (guard (needDateTime v) $>
        S.invokeFunction "TIMESTAMP" (S.string v : L.Nil))
    <> (guard (needInterval v) $>
        S.invokeFunction "INTERVAL" (S.string v : L.Nil))

  lower ∷ S.Sql → S.Sql
  lower = S.invokeFunction "LOWER" ∘ pure

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

--------------------------------------------------------------------------------
-- Interpreter entry point
--------------------------------------------------------------------------------

queryToSql
  ∷ L.List S.Sql
  → SS.SearchQuery
  → S.FUPath
  → S.Sql
queryToSql fields query tablePath =
  S.buildSelect
    $ (S._isDistinct .~ isDistinct)
    ∘ (S._projections .~ topFields)
    ∘ (S._relations ?~ S.TableRelation { alias: Nothing, tablePath })
    ∘ (S._filter ?~ filter)
  where
  topFields =
    map (S.Projection ∘ { expr: _, alias: Nothing}) $ L.filter isTopField fields

  isDistinct = F.any needDistinct $ map flattenIndex fields

  filter =
    ands
    $ map ors
    $ unwrap
    $ map (termToSql fields) query

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testSuite ∷ ∀ e. TestSuite e
testSuite =
  suite "purescript-search interpreter tests" do
    test "dummy" do
      Assert.equal true true
