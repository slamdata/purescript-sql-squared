module Test.Gen where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Either as E

import Test.StrongCheck ((<?>))
import Test.StrongCheck as SC
import Test.StrongCheck.Arbitrary as A

import SqlSquare (Sql, arbitrarySqlOfSize, decodeJson, encodeJson, print, tokenize)

newtype ArbSql = ArbSql Sql

instance arbitraryArbSql ∷ A.Arbitrary ArbSql where
  arbitrary = map ArbSql $ arbitrarySqlOfSize 3

testJsonSerialization ∷ ∀ r. Eff (TestEffects r) Unit
testJsonSerialization =
  SC.quickCheck' 50 \(ArbSql sql) → case decodeJson $ encodeJson sql of
    E.Right res →
      res == sql <?> "Mismatch:\n" <> print sql <> "\n" <> print res
    E.Left err →
      SC.Failed $ "Argonaut codecs  error: " <> err <> " \n" <> print sql

testTokenizer ∷ ∀ r. Eff (TestEffects r) Unit
testTokenizer =
  SC.quickCheck' 50 \(ArbSql sql) → case tokenize $ print sql of
    E.Left err →
      SC.Failed $ "Tokenizer  error: " <> show err <> " \n" <> print sql
    E.Right _ →
      SC.Success


type TestEffects r =
  ( err ∷ EXCEPTION
  , random ∷ RANDOM
  , console ∷ CONSOLE
  | r)

test ∷ ∀ r. Eff (TestEffects r) Unit
test = do
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization
  testJsonSerialization

  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
  testTokenizer
