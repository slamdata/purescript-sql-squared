module Test.Parse where

import Prelude

import Data.Either as E
import Data.Foldable as F
import SqlSquared (parseQuery, SqlQuery)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert
import Test.Unit.Console as Console

testPrintParse ∷ ∀ e. E.Either String String → TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testPrintParse = case _ of
  E.Left s → do
    test s case parseQuery s of
      E.Left err → pure unit
      E.Right (sql ∷ SqlQuery) → Assert.assert "Parse succeeded" false
  E.Right s → do
    test s case parseQuery s of
      E.Left err → Assert.assert (show err) false
      E.Right (sql ∷ SqlQuery) → pure unit

inputs ∷ Array (E.Either String String)
inputs =
  [ E.Right """select a from `/f` where b is not between 1 and (2..3)"""
  , E.Right """select foo, bar from (select foo, bar from (select foo, bar from `/baz`)as t) as d"""
  , E.Right """select (1 + 2) * 3 + 2"""
  , E.Right """select 1 + 2 * 3 ^ 1.2 / 14 + Minimum(12, 23)"""
  , E.Right """select ("12" || "12" || "12")"""
  , E.Right """select date("12-12-12") from `/fo` cross join `/bar`"""
  , E.Right """Select foo as bar from `/test/db/collection`"""
  , E.Right """Select `foo`, `bar`[*] from `/test` join `/test2` on baz where doo = (12 + 23)"""
  , E.Right """foo := 12; select * from `/test` group by baz"""
  , E.Right """select 1"""
  , E.Right """select (1, 2)"""
  , E.Right """foo := [1, 2]; select 1"""
  , E.Right """foo := 1; bar := 2; select [] """
  , E.Right """select foo from `/bar` order by zoo desc"""
  , E.Right """select distinct a from `/f`"""
  , E.Right """select a from /* trololo */ `/db`"""
  , E.Right """-- comment
select 12
"""
  , E.Right """import foo; select * from `/test`"""
  , E.Right """create function foo(:bar) begin :bar + 2 end; select * from `/test` where foo = foo(42)"""
  , E.Right """select :where"""
  , E.Right """foo.`_id`"""
  , E.Left  """foo._id"""
  , E.Right """select * from foo INNER JOIN bar on baz"""
  ]

testSuite ∷ ∀ e. TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testSuite = do
  suite "parsers" do
    F.traverse_ testPrintParse inputs
