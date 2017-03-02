module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NE
import Data.Path.Pathy as Pt
import Debug.Trace (traceAnyA)
import Data.Tuple (Tuple(..))
import SqlSquare.AST as S

someExpr ∷ S.Sql
someExpr = S.invokeFunction_ "foo" $ pure $ S.num_ 12.0

otherExpr ∷ S.Sql
otherExpr =
  S.select_
    false
    [ S.project_ (S.ident_ "foo") # S.as_ "field"
    , S.project_ $ S.splice_ $ Just $ S.binop_ S.FieldDeref (S.ident_ "bar") (S.ident_ "baz")
    ]
    ( map
        (S.TableRelationAST <<< { alias: Nothing, tablePath: _ } <<< Right)
        $ Pt.parseAbsFile "/mongo/testDb/patients" )
    ( Just $ S.binop_ S.Eq (S.ident_ "quux") (S.num_ 12.0) )
    ( Just $ S.groupBy_ [ S.ident_ "zzz" ] # S.having_ ( S.binop_ S.Gt (S.ident_ "ooo") ( S.int_ 2)) )
    ( Just $ S.OrderBy $ NE.singleton $ Tuple S.ASC (S.ident_ "zzz") )


main ∷ ∀ e. Eff e Unit
main = do
  traceAnyA someExpr
  traceAnyA $ S.print someExpr
  traceAnyA $ S.print otherExpr
