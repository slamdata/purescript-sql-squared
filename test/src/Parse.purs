module Test.Parse where

import Prelude

import Data.Either as E
import Data.Foldable as F
import SqlSquared (parseQuery, SqlQuery)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert
import Test.Unit.Console as Console

testPrintParse ∷ ∀ e. String → TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testPrintParse s =
  test s case parseQuery s of
    E.Left err → Assert.assert (show err) false
    E.Right (sql ∷ SqlQuery) → pure unit

inputs ∷ Array String
inputs =
  [ """select a from `/f` where b is not between 1 and (2..3)"""
  , """select foo, bar from (select foo, bar from (select foo, bar from `/baz`)as t) as d"""
  , """select (1 + 2) * 3 + 2"""
  , """select 1 + 2 * 3 ^ 1.2 / 14 + Minimum(12, 23)"""
  , """select ("12" || "12" || "12")"""
  , """select date("12-12-12") from `/fo` cross join `/bar`"""
  , """Select foo as bar from `/test/db/collection`"""
  , """Select `foo`, `bar`[*] from `/test` join `/test2` on baz where doo = (12 + 23)"""
  , """foo := 12; select * from `/test` group by baz"""
  , """select 1"""
  , """select (1, 2)"""
  , """foo := [1, 2]; select 1"""
  , """foo := 1; bar := 2; select [] """
  , """select foo from `/bar` order by zoo desc"""
  , """select distinct a from `/f`"""
  , """select a from /* trololo */ `/db`"""
  , """-- comment
select 12
"""
  , """import foo; select * from `/test`"""
  , """create function foo(:bar) begin :bar + 2 end; select * from `/test` where foo = foo(42)"""
  , """select :where"""
  ]

testSuite ∷ ∀ e. TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testSuite = do
  suite "parsers" do
    F.traverse_ testPrintParse inputs
