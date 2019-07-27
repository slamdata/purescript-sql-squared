module SqlSquared
  ( Sql
  , SqlQuery
  , SqlModule
  , print
  , printPretty
  , printQuery
  , printQueryPretty
  , printModule
  , printModulePretty
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
import Data.Functor.Mu (Mu)
import Data.Json.Extended as EJ
import Data.Traversable (traverse)
import Matryoshka (cata, anaM)
import SqlSquared.Constructors (array, as, as', binop, bool, buildSelect, groupBy, having, hugeNum, ident, ident', int, invokeFunction, invokeFunction', let', let_, map_, match, match', null, num, parens, projection, select, select', set, splice, string, switch, switch', then_, unop, var, when) as Constructors
import SqlSquared.Lenses (_ArrayLiteral, _Binop, _BoolLiteral, _Case, _DecimalLiteral, _ExprRelation, _GroupBy, _Identifier, _IntLiteral, _InvokeFunction, _JoinRelation, _Let, _Literal, _MapLiteral, _Match, _NullLiteral, _OrderBy, _Parens, _Projection, _Select, _SetLiteral, _Splice, _StringLiteral, _Switch, _TableRelation, _Unop, _Var, _VarRelation, _alias, _aliasName, _args, _bindTo, _cases, _clause, _cond, _else, _expr, _filter, _groupBy, _having, _ident, _in, _isDistinct, _joinType, _keys, _left, _lhs, _name, _op, _orderBy, _projections, _relations, _rhs, _right, _tablePath) as Lenses
import SqlSquared.Parser (Literal(..), PositionedToken, parse, parseModule, parseQuery, prettyParse) as Parser
import SqlSquared.Signature (type (×), BinaryOperator(..), BinopR, Case(..), ExprRelR, FunctionDeclR, GroupBy(..), Ident(..), InvokeFunctionR, JoinRelR, JoinType(..), LetR, MatchR, OrderBy(..), OrderType(..), Projection(..), Relation(..), SelectR, SqlDeclF(..), SqlF(..), SqlModuleF(..), SqlQueryF(..), SwitchR, TableRelR, UnaryOperator(..), UnopR, VarRelR, binopFromString, binopToString, genBinaryOperator, genCase, genGroupBy, genJoinType, genOrderBy, genOrderType, genProjection, genRelation, genSqlDeclF, genSqlF, genSqlModuleF, genSqlQueryF, genUnaryOperator, joinTypeFromString, orderTypeFromString, printBinaryOperator, printCase, printGroupBy, printIdent, printJoinType, printOrderBy, printOrderType, printProjection, printRelation, printSqlDeclF, printSqlF, printSqlFPretty, printSqlModuleF, printSqlQueryF, printUnaryOperator, unopFromString, unopToString, (×), (∘), (⋙)) as Sig

type Sql = Mu (Sig.SqlF EJ.EJsonF)

type SqlQuery = Sig.SqlQueryF Sql

type SqlModule = Sig.SqlModuleF Sql

print ∷ Sql → String
print = cata $ Sig.printSqlF EJ.renderEJsonF

printQuery ∷ SqlQuery → String
printQuery = Sig.printSqlQueryF <<< map print

printModule ∷ SqlModule → String
printModule = Sig.printSqlModuleF <<< map print

printPretty ∷ Sql → String
printPretty = cata $ Sig.printSqlFPretty EJ.renderEJsonF

printQueryPretty ∷ SqlQuery → String
printQueryPretty = Sig.printSqlQueryF <<< map printPretty

printModulePretty ∷ SqlModule → String
printModulePretty = Sig.printSqlModuleF <<< map printPretty

genSql ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ m Sql
genSql = Gen.sized $ anaM (Sig.genSqlF EJ.arbitraryEJsonF)

genSqlQuery ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ m SqlQuery
genSqlQuery =
  Gen.sized $ traverse (flip Gen.resize genSql <<< const) <=< Sig.genSqlQueryF

genSqlModule ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ m SqlModule
genSqlModule =
  Gen.sized $ traverse (flip Gen.resize genSql <<< const) <=< Sig.genSqlModuleF
