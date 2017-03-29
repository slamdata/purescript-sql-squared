-- | In this example `purescript-search` query is interpreted to Sql²
-- | using additional `List Sql` with projections (see `Test.Argonaut` to find out how to
-- | get it)
module Test.Search where

import Prelude

import Control.Alt ((<|>))
import Control.MonadZero (guard)

import Data.Argonaut as JS
import Data.Either (Either(..), fromRight)
import Data.Foldable as F
import Data.Int as Int
import Data.Lens ((.~), (?~))
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), isJust, fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Path.Pathy as Pt
import Data.String as Str
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX
import Data.Json.Extended.Signature as EJ

import Global (readFloat, isNaN)

import SqlSquare as S
import SqlSquare.Utils ((∘), (×))

import Matryoshka (Algebra, Coalgebra, Transform, ana, cata, transAna)

import Partial.Unsafe (unsafePartial)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert

import Test.Argonaut as Ar

import Text.SlamSearch (mkQuery)
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

labelString ∷ SS.Label → String
labelString = case _ of
  SS.Meta l → l
  SS.Common l → l

identOrString ∷ ∀ a. (S.SqlF EJ.EJsonF) a → Maybe String
identOrString = case _ of
  S.Ident s → Just s
  S.Literal (EJ.String s) → Just s
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

topFieldF ∷ Algebra (S.SqlF EJ.EJsonF) TopFieldMark
topFieldF = case _ of
  S.Splice Nothing → Init
  S.Ident _ → TopField
  S.Literal (EJ.Integer _) → TopField
  S.Literal (EJ.String _) → TopField
  S.Binop { op: S.FieldDeref, lhs: Init, rhs: TopField } → TopField
  S.Binop { op: S.IndexDeref, lhs: Init, rhs: TopField } → TopField
  _ → NotTopField

isTopField ∷ S.Sql → Boolean
isTopField = isTop ∘ cata topFieldF

--------------------------------------------------------------------------------
-- Flattening all array derefs ( `foo[1]` → `foo[*]` )
--------------------------------------------------------------------------------

flattenIndexF ∷ ∀ t. Transform t (S.SqlF EJ.EJsonF) (S.SqlF EJ.EJsonF)
flattenIndexF = case _ of
  S.Binop { op: S.IndexDeref, lhs } → S.Unop { op: S.FlattenArrayValues, expr: lhs }
  s → s

flattenIndex ∷ S.Sql → S.Sql
flattenIndex = transAna flattenIndexF

--------------------------------------------------------------------------------
-- Searching for flatten values ({*}, [*])
--------------------------------------------------------------------------------

needDistinctF ∷ Algebra (S.SqlF EJ.EJsonF) Boolean
needDistinctF = case _ of
  S.SetLiteral ns → F.or ns
  S.Literal (EJ.Array ns) → F.or ns
  S.Literal (EJ.Map (EJ.EJsonMap tpls)) → F.any (\(a × b) → a || b) tpls
  S.Splice Nothing → false
  S.Splice (Just a) → a
  S.Binop { lhs, rhs } → lhs || rhs
  S.Unop { op: S.FlattenArrayValues } → true
  S.Unop { op: S.FlattenMapValues } → true
  S.Unop { expr } → expr
  S.Ident _ → false
  S.InvokeFunction { args } → F.or args
  S.Match { expr, cases, else_ } →
    expr || F.any (\(S.Case { cond, expr: e }) → e || cond) cases || fromMaybe false else_
  S.Switch { cases, else_ } →
    F.any (\(S.Case { cond, expr }) → cond || expr) cases || fromMaybe false else_
  S.Let { bindTo, in_ } →
    bindTo || in_
  S.Literal _ → false
  S.Vari _ → false
  S.Parens a → a
  S.Select { projections, filter } →
    F.any (\(S.Projection { expr }) → expr) projections || fromMaybe false filter

needDistinct ∷ S.Sql → Boolean
needDistinct = cata needDistinctF

--------------------------------------------------------------------------------
-- Interpretation
--------------------------------------------------------------------------------

extractFields ∷ SS.Term → Maybe S.Sql
extractFields (SS.Term { labels })
  | L.null labels = Nothing
  | otherwise = Just $ ana labelToFieldF $ map labelString $ L.reverse labels

termToSql ∷ L.List S.Sql → SS.Term → S.Sql
termToSql fs (SS.Term { include, predicate, labels})
  | not include =
      S.unop S.Not $ termToSql fields $ SS.Term { include: true, predicate, labels}
  | otherwise =
      ors
      $ flip predicateToSql predicate
      <$> (if L.null labels then fs else pure $ labelsToField labels)


labelToFieldF ∷ Coalgebra (S.SqlF EJ.EJsonF) (L.List String)
labelToFieldF = case _ of
  L.Nil → S.Splice Nothing
  hd : L.Nil → case toInt hd of
    Just i → S.Literal (EJ.Integer i)
    Nothing → S.Ident hd
  hd : tl → case toInt hd of
    Just i → S.Binop { op: S.IndexDeref, lhs: tl, rhs: pure hd }
    Nothing → case hd of
      "[*]" → S.Unop { op: S.FlattenArrayValues, expr: tl }
      "{*}" → S.Unop { op: S.FlattenMapValues, expr: tl }
      "*" → S.Unop { op: S.FlattenMapValues, expr: tl }
      a → S.Binop { op: S.FieldDeref, lhs: tl, rhs: pure hd }
  where
  toInt ∷ String → Maybe Int
  toInt s =
    (Int.fromString s)
    <|> (Str.stripSuffix (Str.Pattern "]") s >>= Str.stripPrefix (Str.Pattern "[") >>= Int.fromString)


labelsToField ∷ L.List SS.Label → S.Sql
labelsToField = ana labelToFieldF ∘ map labelString ∘ L.reverse

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
queryToSql fs query path =
  S.buildSelect
    $ (S._isDistinct .~ isDistinct)
    ∘ (S._projections .~ topFields)
    ∘ (S._relations ?~ S.TableRelation { alias: Nothing, tablePath: path })
    ∘ (S._filter ?~ filter)
  where
  topFields =
    map (S.Projection ∘ { expr: _, alias: Nothing}) $ L.filter isTopField fs

  isDistinct = needDistinct filter

  filter =
    ands
    $ map ors
    $ unwrap
    $ map (termToSql $ map flattenIndex fs) query

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

fields ∷ L.List S.Sql
fields = Ar.allFields jarray
  where
  jarray ∷ JS.JArray
  jarray = map (unsafePartial fromRight ∘ JS.jsonParser) jsonStrings

  jsonStrings ∷ Array String
  jsonStrings =
    [ """{"foo": 1, "bar": 2}"""
    , """{"foo": [1, 2], "bar": null}"""
    , """{"foo": 3, "bar": { "valid": false, "value": "baz" } }"""
    ]

searchQueries ∷ L.List SS.SearchQuery
searchQueries =
  F.foldMap (F.foldMap pure) $  map mkQuery searchStrings
  where
  searchStrings ∷ L.List String
  searchStrings =
    """ba"""
    : """foo:"[*]":2"""
    : """bar:>1"""
    : """false"""
    : """bar:valid:=false"""
    : """"non-existing":foo"""
    : L.Nil

expectedOutput ∷ L.List String
expectedOutput =
  """SELECT DISTINCT *.`bar`, *.`foo` FROM `/mongo/testDb/patients` WHERE (((search(*.`bar`,"^.*ba.*$",true)) OR ((search(*.`foo`,"^.*ba.*$",true))) OR ((search(*.`bar`.`valid`,"^.*ba.*$",true))) OR ((search(*.`bar`.`value`,"^.*ba.*$",true))) OR ((search(*.`foo`[*],"^.*ba.*$",true))) OR ((search(*.`foo`[*],"^.*ba.*$",true)))))"""
  : """SELECT DISTINCT *.`bar`, *.`foo` FROM `/mongo/testDb/patients` WHERE (((search(`foo`[*],"^.*2.*$",true) OR (`foo`[*] = 2.0) OR (`foo`[*] = 2))))"""
  : """SELECT *.`bar`, *.`foo` FROM `/mongo/testDb/patients` WHERE (((LOWER(`bar`) > LOWER("1") OR (`bar` > 1.0) OR (`bar` > 1))))"""
  : """SELECT DISTINCT *.`bar`, *.`foo` FROM `/mongo/testDb/patients` WHERE ((search(*.`bar`,"^.*false.*$",true) OR (*.`bar` = false) OR (search(*.`foo`,"^.*false.*$",true) OR (*.`foo` = false)) OR (search(*.`bar`.`valid`,"^.*false.*$",true) OR (*.`bar`.`valid` = false)) OR (search(*.`bar`.`value`,"^.*false.*$",true) OR (*.`bar`.`value` = false)) OR (search(*.`foo`[*],"^.*false.*$",true) OR (*.`foo`[*] = false)) OR (search(*.`foo`[*],"^.*false.*$",true) OR (*.`foo`[*] = false))))"""
  : """SELECT *.`bar`, *.`foo` FROM `/mongo/testDb/patients` WHERE (((LOWER(`bar`.`valid`) = LOWER("false") OR (`bar`.`valid` = false))))"""
  : """SELECT *.`bar`, *.`foo` FROM `/mongo/testDb/patients` WHERE ((((search(`non-existing`,"^.*foo.*$",true)))))"""
  : L.Nil

tablePath ∷ S.FUPath
tablePath = Right $ unsafePartial fromJust $ Pt.parseAbsFile "/mongo/testDb/patients"

testSuite ∷ ∀ e. TestSuite e
testSuite =
  suite "purescript-search interpreter tests" do
    test "search query is interpreted correctly"
      let
         querySqls = map (\sq → queryToSql fields sq tablePath) searchQueries
         querySqlsStrings = map S.print querySqls
      in
        void $ L.zipWithA Assert.equal expectedOutput querySqlsStrings
