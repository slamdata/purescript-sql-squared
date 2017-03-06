module SqlSquare.Case where

import Prelude

import Data.Newtype (class Newtype)

import Matryoshka (Algebra)

newtype Case a = Case { cond ∷ a, expr ∷ a }

derive instance functorCase ∷ Functor Case
derive instance newtypeCase ∷ Newtype (Case a) _
derive instance eqCase ∷ Eq a ⇒ Eq (Case a)
derive instance ordCase ∷ Ord a ⇒ Ord (Case a)

printCase ∷ Algebra Case String
printCase (Case { cond, expr }) = " when " <> cond <> " then " <> expr
