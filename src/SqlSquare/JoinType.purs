module SqlSquare.JoinType where

import Prelude

data JoinType
  = LeftJoin
  | RightJoin
  | InnerJoin
  | FullJoin

printJoinType ∷ JoinType → String
printJoinType = case _ of
  LeftJoin → "left join"
  RightJoin → "right join"
  FullJoin → "full join"
  InnerJoin → "inner join"

derive instance eqJoinType ∷ Eq JoinType
derive instance ordJoinType ∷ Ord JoinType
