module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

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

import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

import Test.Constructors as Constructors
import Test.Argonaut as Argonaut

type Effects =
  ( testOutput ∷ TESTOUTPUT
  , avar ∷ AVAR
  , console ∷ CONSOLE
  )

main ∷ Eff Effects Unit
main = runTest do
  Constructors.testSuite
  Argonaut.testSuite
--  traceAnyA someExpr
--  traceAnyA $ S.print someExpr
--  traceAnyA $ S.print otherExpr
--  traceAnyA $ S.print thirdExpr
--  traceAnyA $ S.print field
--  traceAnyA $ S.print $ jcursorToSql $ JField "foo" $ JIndex 1 $ JIndex 2 $ JField "bar" $ JCursorTop
