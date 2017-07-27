module Test.Parse where

import Prelude

import Data.Either as E
import SqlSquared (parseQuery, SqlQuery)
import SqlSquared.Parser (prettyParse)

import Test.Queries as Q
import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert
import Test.Unit.Console as Console

parseSucc ∷ ∀ e. String → String → TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
parseSucc n s =
  test n case prettyParse parseQuery s of
    E.Left err → Assert.assert ("\n" <> err) false
    E.Right (sql ∷ SqlQuery) → pure unit

parseFail ∷ ∀ e. String → String → TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
parseFail n s =
  test n case parseQuery s of
    E.Left err → pure unit
    E.Right (sql ∷ SqlQuery) → Assert.assert s false

testSuite ∷ ∀ e. TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testSuite = suite "parsers" do
  parseSucc "t1" """
    select a from `/f` where b is not between 1 and (2..3)
  """

  parseSucc "t2" """
    select foo, bar from (select foo, bar from (select foo, bar from `/baz`)as t) as d
  """

  parseSucc "t3" """
    select (1 + 2) * 3 + 2
  """

  parseSucc "t4" """
    select 1 + 2 * 3 ^ 1.2 / 14 + Minimum(12, 23)
  """

  parseSucc "t5" """
    select ("12" || "12" || "12")
  """

  parseSucc "t6" """
    select date("12-12-12") from `/fo` cross join `/bar`
  """

  parseSucc "t7" """
    Select foo as bar from `/test/db/collection`
  """

  parseSucc "t8" """
    Select `foo`, `bar`[*] from `/test` join `/test2` on baz where doo = (12 + 23)
  """

  parseSucc "t9" """
    foo := 12; select * from `/test` group by baz
  """

  parseSucc "t10" """
    select 1
  """

  parseSucc "t11" """
    select (1, 2)
  """

  parseSucc "t12" """
    foo := [1, 2]; select 1
  """

  parseSucc "t13" """
    foo := 1; bar := 2; select []
  """

  parseSucc "t14" """
    select foo from `/bar` order by zoo desc
  """

  parseSucc "t15" """
    select distinct a from `/f`
  """

  parseSucc "t16" """
    select a from /* trololo */ `/db`
  """

  parseSucc "t17" """
    -- comment
    select 12
  """

  parseSucc "t18" """
    import foo; select * from `/test`
  """

  parseSucc "t19" """
    create function foo(:bar) begin :bar + 2 end; select * from `/test` where foo = foo(42)
  """

  parseSucc "t20" """
    select :where
  """

  parseSucc "t21" """
    foo.`_id`
  """

  parseFail "t22" """
    foo._id
  """

  parseSucc "t23" """
    select * from foo JOIN bar on baz
  """

  parseSucc "t24" """
    select * from foo FULL JOIN bar on baz
  """

  parseSucc "t25" """
    select * from foo FULL OUTER JOIN bar on baz
  """

  parseSucc "t26" """
    select * from foo INNER JOIN bar on baz
  """

  parseSucc "t27" """
    select * from foo LEFT OUTER JOIN bar on baz
  """

  parseSucc "t28" """
    select * from foo LEFT JOIN bar on baz
  """

  parseSucc "t29" """
    select * from foo RIGHT OUTER JOIN bar on baz
  """

  parseSucc "t30" """
    select * from foo RIGHT JOIN bar on baz
  """

  parseSucc "t31" """
    industry
  """

  parseSucc "q1" Q.q1
  parseSucc "q2" Q.q2
  parseSucc "q3" Q.q3
  parseSucc "q4" Q.q4
  parseSucc "q5" Q.q5
  parseSucc "q6" Q.q6
  parseSucc "q7" Q.q7
  parseSucc "q8" Q.q8
  parseSucc "q9" Q.q9
  parseSucc "q10" Q.q10
  parseSucc "q11" Q.q11
  parseSucc "q12" Q.q12
  parseSucc "q13" Q.q13
  parseSucc "q14" Q.q14
  parseSucc "q15" Q.q15
  parseSucc "q16" Q.q16
  parseSucc "q17" Q.q17
  parseSucc "q18" Q.q18
  parseSucc "q19" Q.q19
  parseSucc "q20" Q.q20
  parseSucc "q21" Q.q21
