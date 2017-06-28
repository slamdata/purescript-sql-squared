module SqlSquared
  ( Sql
  , SqlQuery
  , SqlModule
  , print
  , printQuery
  , printModule
  , encodeJson
  , encodeJsonQuery
  , encodeJsonModule
  , decodeJson
  , decodeJsonQuery
  , decodeJsonModule
  , arbitrarySqlOfSize
  , arbitrarySqlQueryOfSize
  , arbitrarySqlModuleOfSize
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

type SqlQuery = Sig.SqlQueryF Sql

type SqlModule = Sig.SqlModuleF Sql

print ∷ Sql → String
print = cata $ Sig.printSqlF EJ.renderEJsonF

printQuery ∷ SqlQuery → String
printQuery = Sig.printSqlQueryF <<< map print

printModule ∷ SqlModule → String
printModule = Sig.printSqlModuleF <<< map print

encodeJson ∷ Sql → J.Json
encodeJson = cata $ Sig.encodeJsonSqlF EJ.encodeJsonEJsonF

encodeJsonQuery ∷ SqlQuery → J.Json
encodeJsonQuery = Sig.encodeJsonSqlQueryF <<< map encodeJson

encodeJsonModule ∷ SqlModule → J.Json
encodeJsonModule = Sig.encodeJsonSqlModuleF <<< map encodeJson

decodeJson ∷ J.Json → Either String Sql
decodeJson = anaM $ Sig.decodeJsonSqlF EJ.decodeJsonEJsonF

decodeJsonQuery ∷ J.Json → Either String SqlQuery
decodeJsonQuery = traverse decodeJson <=< Sig.decodeJsonSqlQueryF

decodeJsonModule ∷ J.Json → Either String SqlModule
decodeJsonModule = traverse decodeJson <=< Sig.decodeJsonSqlModuleF

arbitrarySqlOfSize ∷ Int → Gen.Gen Sql
arbitrarySqlOfSize = anaM $ Sig.arbitrarySqlF EJ.arbitraryEJsonF

arbitrarySqlQueryOfSize ∷ Int → Gen.Gen SqlQuery
arbitrarySqlQueryOfSize = traverse arbitrarySqlOfSize <=< Sig.arbitrarySqlQueryF

arbitrarySqlModuleOfSize ∷ Int → Gen.Gen SqlModule
arbitrarySqlModuleOfSize = traverse arbitrarySqlOfSize <=< Sig.arbitrarySqlModuleF
