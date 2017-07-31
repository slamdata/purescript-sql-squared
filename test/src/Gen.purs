module Test.Gen where

import Prelude

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Array ((..))
import Data.Either as E

import Test.StrongCheck ((<?>))
import Test.StrongCheck as SC
import Test.StrongCheck.Arbitrary as A
import Test.Unit.Console as Console

import SqlSquared (SqlQuery, arbitrarySqlQueryOfSize, decodeJsonQuery, encodeJsonQuery, printQuery, tokenize)

newtype ArbSql = ArbSql SqlQuery

instance arbitraryArbSql ∷ A.Arbitrary ArbSql where
  arbitrary = map ArbSql $ arbitrarySqlQueryOfSize 3

newtype ParseableSql = ParseableSql SqlQuery

testJsonSerialization ∷ ∀ r. Eff (TestEffects r) Unit
testJsonSerialization =
  SC.quickCheck' 50 \(ArbSql sql) → case decodeJsonQuery $ encodeJsonQuery sql of
    E.Right res →
      res == sql <?> "Mismatch:\n" <> printQuery sql <> "\n" <> printQuery res
    E.Left err →
      SC.Failed $ "Argonaut codecs  error: " <> err <> " \n" <> printQuery sql

testTokenizer ∷ ∀ r. Eff (TestEffects r) Unit
testTokenizer =
  SC.quickCheck' 50 \(ArbSql sql) → case tokenize $ printQuery sql of
    E.Left err →
      SC.Failed $ "Tokenizer  error: " <> show err <> " \n" <> printQuery sql
    E.Right _ →
      SC.Success

type TestEffects r =
  ( exception ∷ EXCEPTION
  , random ∷ RANDOM
  , console ∷ CONSOLE
  , testOutput ∷ Console.TESTOUTPUT
  | r)

test ∷ ∀ r. Eff (TestEffects r) Unit
test = do
  Console.printLabel "\n\n:::::::::: TESTING Argonaut codecs ::::::::::\n\n"
  foreachE (1 .. 20) $ const $ testJsonSerialization
  -- Console.printLabel "\n\n:::::::::: TESTING TOKENIZER ::::::::::\n\n"
  -- foreachE (1 .. 20) $ const $ testTokenizer
