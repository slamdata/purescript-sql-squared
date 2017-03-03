module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)

import Data.Argonaut (JCursor(..))
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NE
import Data.Path.Pathy as Pt
import Debug.Trace (traceAnyA)
import Data.Tuple (Tuple(..))
import SqlSquare.Utils ((∘), (⋙))
import SqlSquare.AST as S
import Data.Lens ((.~), (?~), (<>~))
import Matryoshka (class Recursive, class Corecursive, Coalgebra, ana)


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
        (S.TableRelation ∘ { alias: Nothing, tablePath: _ } ∘ Right)
        $ Pt.parseAbsFile "/mongo/testDb/patients" )
    ( Just $ S.binop_ S.Eq (S.ident_ "quux") (S.num_ 12.0) )
    ( Just $ S.groupBy_ [ S.ident_ "zzz" ] # S.having_ ( S.binop_ S.Gt (S.ident_ "ooo") ( S.int_ 2)) )
    ( Just $ S.OrderBy $ NE.singleton $ Tuple S.ASC (S.ident_ "zzz") )


thirdExpr ∷ S.Sql
thirdExpr =
  S.buildSelect
    $ (S._isDistinct .~ true)
    ∘ (S._projections <>~ (L.singleton $ S.project_ (S.ident_ "foo") # S.as_ "field"))
    ∘ (S._projections <>~
         (L.singleton
          $ S.project_
          $ S.splice_
          $ Just
          $ S.binop_
              S.FieldDeref
              (S.ident_ "bar")
              (S.ident_ "baz")))
    ∘ (S._relations .~
         (map (S.TableRelation ∘ { alias: Nothing, tablePath: _} ∘ Right)
           $ Pt.parseAbsFile "/mongo/testDb/patients"))
    ∘ (S._filter ?~ S.binop_ S.Eq (S.ident_ "quux") (S.num_ 12.0))
    ∘ (S._groupBy  ?~
         (S.groupBy_ [ S.ident_ "zzz" ] # S.having_ (S.binop_ S.Gt (S.ident_ "ooo") (S.int_ 2))))
    ∘ (S._orderBy ?~ S.OrderBy (NE.singleton $ Tuple S.ASC (S.ident_ "zzz")))

field ∷ S.Sql
field = S.binop_ S.FieldDeref (S.splice_ Nothing) (S.ident_ "field")

main ∷ ∀ e. Eff e Unit
main = do
  traceAnyA someExpr
  traceAnyA $ S.print someExpr
  traceAnyA $ S.print otherExpr
  traceAnyA $ S.print thirdExpr
  traceAnyA $ S.print field
  traceAnyA $ S.print $ jcursorToSql $ JField "foo" $ JIndex 1 $ JIndex 2 $ JField "bar" $ JCursorTop
