module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)

import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

import Test.Constructors as Constructors
import Test.Argonaut as Argonaut
import Test.Gen as Gen
import Test.Parse as Parse
import Test.Precedence as Precedence

type Effects =
  ( testOutput ∷ TESTOUTPUT
  , avar ∷ AVAR
  , console ∷ CONSOLE
  , exception ∷ EXCEPTION
  , random ∷ RANDOM
  )

main ∷ Eff Effects Unit
main = do
  runTest do
    Constructors.testSuite
    Argonaut.testSuite
    Parse.testSuite
    Precedence.testSuite

  Gen.test
