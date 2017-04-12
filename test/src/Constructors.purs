module Test.Constructors where

import Prelude

import Data.List as L
import Data.Lens ((.~), (<>~), (?~))
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NE
import Data.Either as E
import Data.Path.Pathy as Pt

import SqlSquare as S
import SqlSquare.Utils ((×), (∘))

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert

selectQuery ∷ S.Sql
selectQuery =
  S.select
    true
    [ S.projection (S.ident "foo") # S.as "field"
    , S.projection $ S.splice $ Just $ S.binop S.FieldDeref (S.ident "bar") (S.ident "baz")
    ]
    ( Just $ S.TableRelation
        { alias: Nothing
        , path: E.Left
          $ Pt.rootDir
          Pt.</> Pt.dir "mongo"
          Pt.</> Pt.dir "testDb"
          Pt.</> Pt.file "patients"
        })
    ( Just $ S.binop S.Eq (S.ident "quux") (S.num 12.0) )
    ( Just $ S.groupBy [ S.ident "zzz" ] # S.having ( S.binop S.Gt (S.ident "ooo") ( S.int 2)) )
    ( Just $ S.OrderBy $ NE.singleton $ S.ASC × (S.ident "zzz") )

buildSelectQuery ∷ S.Sql
buildSelectQuery =
  S.buildSelect
    $ (S._isDistinct .~ true)
    ∘ (S._projections <>~
         (L.singleton
          $ S.projection
          $ S.splice
          $ Just
          $ S.binop
              S.FieldDeref
              (S.ident "bar")
              (S.ident "baz")))
    ∘ (S._projections <>~  (L.singleton $ S.projection (S.ident "foo") # S.as "field"))
    ∘ (S._relations ?~
         (S.TableRelation
           { alias: Nothing
           , path: E.Left
             $ Pt.rootDir
             Pt.</> Pt.dir "mongo"
             Pt.</> Pt.dir "testDb"
             Pt.</> Pt.file "patients"
           }))

    ∘ (S._filter ?~ S.binop S.Eq (S.ident "quux") (S.num 12.0))
    ∘ (S._groupBy ?~
         (S.groupBy [ S.ident "zzz" ] # S.having (S.binop S.Gt (S.ident "ooo") (S.int 2))))
    ∘ (S._orderBy ?~ S.OrderBy (NE.singleton $ S.ASC × (S.ident "zzz")))

expectedSqlString ∷ String
expectedSqlString =
  "SELECT DISTINCT foo AS field, bar.baz.* FROM `/mongo/testDb/patients` WHERE quux = 12.0 GROUP BY zzz HAVING ooo > 2 ORDER BY zzz ASC"

testSuite ∷ ∀ e. TestSuite e
testSuite = do
  suite "tests for sql constructors" do
    test "constructing select query with multiple arguments"
      $ Assert.equal expectedSqlString $ S.print selectQuery
    test "building select query with lenses"
      $ Assert.equal expectedSqlString $ S.print buildSelectQuery
