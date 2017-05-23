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

import SqlSquared (SqlTop, arbitrarySqlTopOfSize, decodeJsonTop, encodeJsonTop, printTop, tokenize)

newtype ArbSql = ArbSql SqlTop

instance arbitraryArbSql ∷ A.Arbitrary ArbSql where
  arbitrary = map ArbSql $ arbitrarySqlTopOfSize 3

newtype ParseableSql = ParseableSql SqlTop

testJsonSerialization ∷ ∀ r. Eff (TestEffects r) Unit
testJsonSerialization =
  SC.quickCheck' 50 \(ArbSql sql) → case decodeJsonTop $ encodeJsonTop sql of
    E.Right res →
      res == sql <?> "Mismatch:\n" <> printTop sql <> "\n" <> printTop res
    E.Left err →
      SC.Failed $ "Argonaut codecs  error: " <> err <> " \n" <> printTop sql

testTokenizer ∷ ∀ r. Eff (TestEffects r) Unit
testTokenizer =
  SC.quickCheck' 50 \(ArbSql sql) → case tokenize $ printTop sql of
    E.Left err →
      SC.Failed $ "Tokenizer  error: " <> show err <> " \n" <> printTop sql
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
  Console.printLabel "\n\n:::::::::: TESTING TOKENIZER ::::::::::\n\n"
  foreachE (1 .. 20) $ const $ testTokenizer
