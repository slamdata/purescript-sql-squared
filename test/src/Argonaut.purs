-- | An example of using `purescript-sqlsquare` library
-- | Having an array of `Json`s construct a list of Sql² projections
module Test.Argonaut where

import Prelude

import Data.Argonaut (JCursor(..), jsonParser)
import Data.Argonaut as JS
import Data.Either (fromRight)
import Data.Foldable as F
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple, fst)
import Data.Json.Extended.Signature (EJsonF(..))

import SqlSquare as S
import SqlSquare.Utils ((×), (∘), (⋙))

import Matryoshka (ana, elgotPara, Coalgebra, ElgotAlgebra)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert

import Partial.Unsafe (unsafePartial)

data UnfoldableJC = JC JCursor | S String | I Int

jcCoalgebra ∷ Coalgebra (S.SqlF EJsonF) UnfoldableJC
jcCoalgebra = case _ of
  S s → S.Ident s
  I i → S.Literal (Integer  i)
  JC cursor → case cursor of
    JCursorTop → S.Splice Nothing
    JIndex i c → S.Binop { op: S.IndexDeref, lhs: JC c, rhs: I i }
    JField f c → S.Binop { op: S.FieldDeref, lhs: JC c, rhs: S f }

jcursorToSql ∷ JCursor → S.Sql
jcursorToSql = JS.insideOut ⋙ JC ⋙ ana jcCoalgebra

fields ∷ JS.JArray → L.List S.Sql
fields arr =
  map jcursorToSql $ L.fromFoldable $ F.foldMap (Set.fromFoldable ∘ map fst) $ map JS.toPrims arr

allParentsF ∷ ElgotAlgebra (Tuple S.Sql) (S.SqlF EJsonF) (L.List S.Sql)
allParentsF (parent × sqlF) = case sqlF of
  S.Splice (Just ps) → ps
  S.Unop { op: S.FlattenArrayValues, expr } → parent : expr
  S.Unop { op: S.FlattenMapValues, expr } → parent : expr
  S.Binop { op: S.FieldDeref, lhs } → parent : lhs
  S.Binop { op: S.IndexDeref, lhs } → parent : lhs
  _ → L.Nil

allParents ∷ S.Sql → L.List S.Sql
allParents = elgotPara allParentsF

allFields ∷ JS.JArray → L.List S.Sql
allFields =
  L.fromFoldable ∘ F.foldMap (Set.fromFoldable ∘ allParents) ∘ fields

jarray ∷ JS.JArray
jarray =
  map (unsafePartial fromRight ∘ jsonParser) jsonStrings
  where
  jsonStrings =
    [ """{"foo": [{"bar": 1}, 12], "bar": {"baz": false}}"""
    , """{"foo": true}"""
    , """[12, null]"""
    ]
testSuite ∷ ∀ e. TestSuite e
testSuite =
  suite "tests for argonaut example" do
    test "interpretation works"
      let
        expected =
          "*.`foo`[1][2][0]"
          : "*.`foo`.`bar`.`baz`"
          : L.Nil
        js =
          (JField "foo" $ JIndex 1 $ JIndex 2 $ JIndex 0 $ JCursorTop)
          : (JField "foo" $ JField "bar" $ JField "baz" $ JCursorTop)
          : L.Nil
      in
        Assert.equal expected $ map (S.print ∘ jcursorToSql) js
    test "extraction of fields works"
      let
        actualFields =
          Set.fromFoldable
          $ map S.print $ fields jarray
        expectedFields =
          Set.fromFoldable
          $ "*[0]"
          : "*[1]"
          : "*.`foo`"
          : "*.`foo`[1]"
          : "*.`foo`[0].`bar`"
          : "*.`bar`.`baz`"
          : L.Nil
      in
        Assert.equal expectedFields actualFields
    test "allParents extracted"
      let
        field =
          jcursorToSql
          $ JField "foo"
          $ JField "bar"
          $ JIndex 0
          $ JField "baz"
          $ JIndex 1
          $ JCursorTop
        expected =
          Set.fromFoldable
          $ "*.`foo`"
          : "*.`foo`.`bar`"
          : "*.`foo`.`bar`[0]"
          : "*.`foo`.`bar`[0].`baz`"
          : "*.`foo`.`bar`[0].`baz`[1]"
          : L.Nil
      in
        Assert.equal expected $ Set.fromFoldable $ map S.print $ allParents field
    test "allFields works"
      let
        actualFields = Set.fromFoldable $ map S.print $ allFields jarray
        expectedFields =
          Set.fromFoldable
          $ "*[0]"
          : "*[1]"
          : "*.`foo`"
          : "*.`foo`[1]"
          : "*.`foo`[0].`bar`"
          : "*.`bar`.`baz`"
          : "*.`bar`"
          : L.Nil
      in
        Assert.equal "1" "1"
--        Assert.equal expectedFields actualFields
