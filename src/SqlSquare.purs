module SqlSquare
  ( Sql
  , print
  , encodeJson
  , decodeJson
  , arbitrarySqlOfSize
  , module Sig
  , module Lenses
  , module Constructors
  ) where

import Prelude

import Data.Argonaut as J
import Data.Either (Either)
import Data.Functor.Mu (Mu)
import Data.Json.Extended as EJ

import Matryoshka (cata, anaM)

import SqlSquare.Signature as Sig
import SqlSquare.Lenses as Lenses
import SqlSquare.Constructors as Constructors

import Test.StrongCheck.Gen as Gen

type Sql = Mu (Sig.SqlF EJ.EJsonF)

print ∷ Sql → String
print = cata $ Sig.printSqlF EJ.renderEJsonF

encodeJson ∷ Sql → J.Json
encodeJson = cata $ Sig.encodeJsonSqlF EJ.encodeJsonEJsonF

decodeJson ∷ J.Json → Either String Sql
decodeJson = anaM $ Sig.decodeJsonSqlF EJ.decodeJsonEJsonF

arbitrarySqlOfSize ∷ Int → Gen.Gen Sql
arbitrarySqlOfSize = anaM $ Sig.arbitrarySqlF EJ.arbitraryEJsonF
