module SqlSquare.OrderType where

import Prelude

import Data.Argonaut as J
import Data.Either (Either(..))
import Data.List ((:))
import Data.List as L

import Test.StrongCheck.Arbitrary as A
import Test.StrongCheck.Gen as Gen

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

instance encodeJsonOrderType ∷ J.EncodeJson OrderType where
  encodeJson ot =
    "tag" J.:= "order type"
    J.~> "value" J.:= printOrderType ot
    J.~> J.jsonEmptyObject

instance decodeJsonOrderType ∷ J.DecodeJson OrderType where
  decodeJson = J.decodeJson >=> \obj → do
    tag ← obj J..? "tag"
    unless (tag == "order type")
      $ Left "This is not order type"
    (obj J..? "value") >>= orderTypeFromString
