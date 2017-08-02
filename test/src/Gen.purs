module Test.Gen where

import Prelude

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array ((..))
import Data.Either as E
import SqlSquared (SqlQuery, arbitrarySqlQueryOfSize, decodeJsonQuery, encodeJsonQuery, printQuery, tokenize)
import Test.QuickCheck ((<?>))
import Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary as A
import Test.Unit.Console as Console

newtype ArbSql = ArbSql SqlQuery

instance arbitraryArbSql ∷ A.Arbitrary ArbSql where
  arbitrary = map ArbSql $ arbitrarySqlQueryOfSize 3

newtype ParseableSql = ParseableSql SqlQuery

testJsonSerialization ∷ ∀ r. Eff (TestEffects r) Unit
testJsonSerialization =
  QC.quickCheck' 1000 \(ArbSql sql) → case decodeJsonQuery $ encodeJsonQuery sql of
    E.Right res → res == sql <?> "Mismatch:\n" <> printQuery sql <> "\n" <> printQuery res
    E.Left err → QC.Failed $ "Codec error: " <> err <> " \n" <> printQuery sql

testTokenizer ∷ ∀ r. Eff (TestEffects r) Unit
testTokenizer =
  QC.quickCheck' 1000 \(ArbSql sql) → case tokenize $ printQuery sql of
    E.Left err → QC.Failed $ "Tokenizer error: " <> show err <> " \n" <> printQuery sql
    E.Right _ → QC.Success

type TestEffects r =
  ( exception ∷ EXCEPTION
  , random ∷ RANDOM
  , console ∷ CONSOLE
  , testOutput ∷ Console.TESTOUTPUT
  | r
  )

test ∷ ∀ r. Eff (TestEffects r) Unit
test = do
  testJsonSerialization
  testTokenizer
