module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

import Test.Constructors as Constructors
import Test.Argonaut as Argonaut
import Test.Search as Search

type Effects =
  ( testOutput ∷ TESTOUTPUT
  , avar ∷ AVAR
  , console ∷ CONSOLE
  )

main ∷ Eff Effects Unit
main = runTest do
  Constructors.testSuite
  Argonaut.testSuite
  Search.testSuite
