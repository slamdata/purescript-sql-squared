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
  , genSql
  , genSqlQuery
  , genSqlModule
  , module Sig
  , module Lenses
  , module Constructors
  , module Parser
  ) where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut as J
import Data.Either (Either)
import Data.Functor.Mu (Mu)
import Data.Json.Extended as EJ
import Data.Traversable (traverse)
import Matryoshka (cata, anaM)
import SqlSquared.Constructors as Constructors
import SqlSquared.Lenses as Lenses
import SqlSquared.Parser as Parser
import SqlSquared.Signature as Sig

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

genSql ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ m Sql
genSql = Gen.sized $ anaM (Sig.genSqlF EJ.arbitraryEJsonF)

genSqlQuery ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ m SqlQuery
genSqlQuery =
  Gen.sized $ traverse (flip Gen.resize genSql <<< const) <=< Sig.genSqlQueryF

genSqlModule ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ m SqlModule
genSqlModule =
  Gen.sized $ traverse (flip Gen.resize genSql <<< const) <=< Sig.genSqlModuleF
