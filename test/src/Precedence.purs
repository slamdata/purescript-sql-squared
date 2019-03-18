module Test.Precedence where

import Test.Prelude

import Data.Either as E
import Matryoshka (project)
import SqlSquared as S

testParsedSql ∷ (S.Sql → Test) → String → Test
testParsedSql f s =
  case S.prettyParse S.parse s of
    E.Left err → assert ("\n" <> err) false
    E.Right sql → f sql

limitedJoinQuery ∷ String
limitedJoinQuery = "select * from a inner join b on a.id = b.id limit 10"

expectLimit ∷ S.Sql → Boolean
expectLimit sql =
  case project sql of
    (S.Binop { lhs: _, rhs: _, op: S.Limit }) → true
    _ → false

testSuite ∷ Test
testSuite = do
  suite "tests for parser precedence" do
    test "limit should have higher precedence than join condition"
      $ testParsedSql (assert "limit parsed incorrectly" <<< expectLimit) limitedJoinQuery
