module SqlSquare.JoinType where

import Prelude

data JoinType
  = LeftJoin
  | RightJoin
  | InnerJoin
  | FullJoin

printJoinType ∷ JoinType → String
printJoinType = case _ of
  LeftJoin → "LEFT JOIN"
  RightJoin → "RIGHT JOIN"
  FullJoin → "FULL JOIN"
  InnerJoin → "INNER JOIN"

derive instance eqJoinType ∷ Eq JoinType
derive instance ordJoinType ∷ Ord JoinType
