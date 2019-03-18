module Test.Prelude
  ( module Test.Prelude
  , module Prelude
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Test.Assert as Assert
import Test.QuickCheck (unSeed)
import Test.QuickCheck as QC

type Test = ReaderT Int Effect Unit

runTest :: Test -> Effect Unit
runTest = flip runReaderT 0

suite :: String -> Test -> Test
suite = test

test :: String -> Test -> Test
test name run = do
  indent <- ask
  log (mkIndent indent <> name)
  local (_ + 2) run

mkIndent :: Int -> String
mkIndent = power " "

assert :: String -> Boolean -> Test
assert msg = liftEffect <<< Assert.assert' msg

assertEqual :: forall a. Eq a => Show a => { actual :: a, expected :: a } -> Test
assertEqual = liftEffect <<< Assert.assertEqual

quickCheck :: forall prop. QC.Testable prop => prop -> Test
quickCheck prop = liftEffect do
  seed <- QC.randomSeed
  let summary = QC.checkResults (QC.quickCheckPure' seed 100 prop)
  case List.head summary.failures of
    Nothing -> pure unit
    Just err -> throw $ "Property failed (seed " <> show (unSeed err.seed) <> ") failed: \n" <> err.message

failure :: String -> Test
failure = liftEffect <<< throw
