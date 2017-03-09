module SqlSquare.OrderType where

import Prelude

data OrderType = ASC | DESC

printOrderType ∷ OrderType → String
printOrderType = case _ of
  ASC → "ASC"
  DESC → "DESC"

derive instance eqOrderType ∷ Eq OrderType
derive instance ordOrderType ∷ Ord OrderType
