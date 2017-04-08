module Test.Parse where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Either as E
import Data.Foldable as F
import SqlSquare (Sql, print, parse, tokenize)

import Test.Unit.Main (runTest)
import Test.Unit (suite, test, TestSuite)
import Test.Unit.Console as Console

import Debug.Trace as DT

testPrintParse ∷ String → Eff _ Unit
testPrintParse s = do
  Console.printLabel $ "Testing: \n" <> s <> "\n"
  let parsed = parse s
  case parsed of
    E.Left e → Console.printFail $ "Fail: " <> show e <> "\n"
    E.Right (sql ∷ Sql) → do
      Console.printPass "Success: \n"
      DT.traceAnyA sql
      Console.printPass $ print sql
      Console.printPass "\n"

inputs ∷ Array String
inputs =
  [ """select a from `/f` where b is not between 1 and (2..3)"""
  , """select foo, bar from (select foo, bar from (select foo, bar from `/baz`)as t) as d"""
  , """select (1 + 2) * 3 + 2"""
  , """select 1 + 2 * 3 ^ 1.2 / 14 + Minimum(12, 23)"""
  , """select ("12" || "12" || "12")"""
  , """select date("12-12-12") from `/fo` cross join `/bar`"""
  , """Select foo as bar from `/test/db/collection`"""
  , """Select `foo`, `bar`.[*] from `/test` join `/test2` on baz where doo = (12 + 23)"""
  , """:foo := 12; select * from `/test` group by baz"""
  , """select 1"""
  , """select (1, 2)"""
  , """:foo := [1, 2]; select 1"""
  , """:foo := 1; :bar := 2; select [] """
  , """select foo from `/bar` order by zoo desc"""

  ]


testSuite ∷ ∀ e. Eff _ Unit
testSuite = do
  Console.printLabel "\n\n:::::::::: TESTING PARSER ::::::::::\n\n"
  F.for_ inputs testPrintParse
