module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Constructors as Constructors
import Test.Argonaut as Argonaut
import Test.Gen as Gen
import Test.Parse as Parse
import Test.Precedence as Precedence

main ∷ Effect Unit
main = do
  runTest do
    Constructors.testSuite
    Argonaut.testSuite
    Parse.testSuite
    Precedence.testSuite

  Gen.test
