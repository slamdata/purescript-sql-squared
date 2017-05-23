module SqlSquared
  ( Sql
  , SqlTop
  , print
  , printTop
  , encodeJson
  , encodeJsonTop
  , decodeJson
  , decodeJsonTop
  , arbitrarySqlOfSize
  , arbitrarySqlTopOfSize
  , module Sig
  , module Lenses
  , module Constructors
  , module Parser
  ) where

import Prelude

import Data.Argonaut as J
import Data.Either (Either)
import Data.Functor.Mu (Mu)
import Data.Json.Extended as EJ
import Data.Traversable (traverse)

import Matryoshka (cata, anaM)

import SqlSquared.Signature as Sig
import SqlSquared.Lenses as Lenses
import SqlSquared.Constructors as Constructors
import SqlSquared.Parser as Parser

import Test.StrongCheck.Gen as Gen

type Sql = Mu (Sig.SqlF EJ.EJsonF)

type SqlTop = Sig.SqlTopF Sql

print ∷ Sql → String
print = cata $ Sig.printSqlF EJ.renderEJsonF

printTop ∷ SqlTop → String
printTop = Sig.printSqlTopF <<< map print

encodeJson ∷ Sql → J.Json
encodeJson = cata $ Sig.encodeJsonSqlF EJ.encodeJsonEJsonF

encodeJsonTop ∷ SqlTop → J.Json
encodeJsonTop = Sig.encodeJsonSqlTopF <<< map encodeJson

decodeJson ∷ J.Json → Either String Sql
decodeJson = anaM $ Sig.decodeJsonSqlF EJ.decodeJsonEJsonF

decodeJsonTop ∷ J.Json → Either String SqlTop
decodeJsonTop = traverse decodeJson <=< Sig.decodeJsonSqlTopF

arbitrarySqlOfSize ∷ Int → Gen.Gen Sql
arbitrarySqlOfSize = anaM $ Sig.arbitrarySqlF EJ.arbitraryEJsonF

arbitrarySqlTopOfSize ∷ Int → Gen.Gen SqlTop
arbitrarySqlTopOfSize = traverse arbitrarySqlOfSize <=< Sig.arbitrarySqlTopF
