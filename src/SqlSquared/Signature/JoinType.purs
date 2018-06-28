module SqlSquared.Signature.JoinType where

import Prelude

import Control.Monad.Gen as Gen
import Data.Either (Either(..))
import Data.NonEmpty ((:|))

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

joinTypeFromString ∷ String → Either String JoinType
joinTypeFromString = case _ of
  "LEFT JOIN" → Right LeftJoin
  "RIGHT JOIN" → Right RightJoin
  "FULL JOIN" → Right FullJoin
  "INNER JOIN" → Right InnerJoin
  _ → Left "This is not join type"

derive instance eqJoinType ∷ Eq JoinType
derive instance ordJoinType ∷ Ord JoinType

genJoinType ∷ ∀ m. Gen.MonadGen m ⇒ m JoinType
genJoinType = Gen.elements $ LeftJoin :| [ RightJoin, InnerJoin, FullJoin ]
