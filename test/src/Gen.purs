module Test.Gen where

import Prelude

import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Either as E

import Test.StrongCheck ((<?>))
import Test.StrongCheck as SC
import Test.StrongCheck.Arbitrary as A

import SqlSquare (Sql, arbitrarySqlOfSize, decodeJson, encodeJson, print)

import Debug.Trace as DT

newtype ArbSql = ArbSql Sql

instance arbitraryArbSql ∷ A.Arbitrary ArbSql where
  arbitrary = map ArbSql $ arbitrarySqlOfSize 3

testJsonSerialization ∷ ∀ r. Eff (TestEffects r) Unit
testJsonSerialization =
  SC.quickCheck' 50 \(ArbSql sql) → case decodeJson $ encodeJson sql of
    E.Right res →
      if (res /= sql)
      then
        let
          o = DT.spy res
          oo = DT.spy sql
          ooo = DT.spy $ encodeJson res
          oooo = DT.spy $ encodeJson sql
          ooooo = DT.spy $ print res
          oooooo = DT.spy $ print sql
        in
         SC.Failed "Mismatch"
      else
        SC.Success
--      res == sql <?> "Mismatch:\n" <> print sql <> "\n" <> print res
    E.Left err →
      SC.Failed $ "Parse error: " <> err <> " \n" <> print sql

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
