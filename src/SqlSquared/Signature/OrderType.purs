module SqlSquared.Signature.OrderType where

import Prelude

import Control.Monad.Gen as Gen
import Data.Either (Either(..))

data OrderType = ASC | DESC

printOrderType ∷ OrderType → String
printOrderType = case _ of
  ASC → "ASC"
  DESC → "DESC"

orderTypeFromString ∷ String → Either String OrderType
orderTypeFromString = case _ of
  "ASC" → Right ASC
  "DESC" → Right DESC
  _ → Left "This is not order type"

derive instance eqOrderType ∷ Eq OrderType
derive instance ordOrderType ∷ Ord OrderType

genOrderType ∷ ∀ m. Gen.MonadGen m ⇒ m OrderType
genOrderType = Gen.choose (pure ASC) (pure DESC)
