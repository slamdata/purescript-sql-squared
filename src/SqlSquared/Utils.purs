module SqlSquared.Utils where

import Prelude
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.Tuple (Tuple(..))

infixr 4 type Tuple as ×
infixr 1 Tuple as ×
infixr 9 compose as ∘
infixr 4 type Coproduct as ⨁
infixr 5 coproduct as ⨁

composeFlipped ∷ ∀ a b c d. Semigroupoid a ⇒ a b c → a c d → a b d
composeFlipped f g = compose g f

infixr 9 composeFlipped as ⋙
