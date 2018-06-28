module Test.Gen where

import Prelude

import Control.Monad.Gen as Gen
import Data.Either as E
import Effect (Effect)
import SqlSquared (SqlQuery, genSqlQuery, printQuery, tokenize)
import Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary as A

newtype ArbSql = ArbSql SqlQuery

instance arbitraryArbSql ∷ A.Arbitrary ArbSql where
  arbitrary = map ArbSql $ Gen.resize (const 3) genSqlQuery

newtype ParseableSql = ParseableSql SqlQuery

test ∷ Effect Unit
test =
  QC.quickCheck' 1000 \(ArbSql sql) → case tokenize $ printQuery sql of
    E.Left err → QC.Failed $ "Tokenizer error: " <> show err <> " \n" <> printQuery sql
    E.Right _ → QC.Success
