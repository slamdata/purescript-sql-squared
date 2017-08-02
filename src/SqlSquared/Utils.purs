module SqlSquared.Utils where

import Prelude
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.String.Regex (test, replace) as Regex
import Data.String.Regex.Flags (ignoreCase, global) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Data.Tuple (Tuple(..))

infixr 4 type Tuple as ×
infixr 1 Tuple as ×
infixr 9 compose as ∘
infixr 4 type Coproduct as ⨁
infixr 5 coproduct as ⨁

composeFlipped ∷ ∀ a b c d. Semigroupoid a ⇒ a b c → a c d → a b d
composeFlipped f g = compose g f

infixr 9 composeFlipped as ⋙

escapeIdent ∷ String → String → String
escapeIdent delim ident =
  if Regex.test identifier ident
    then ident
    else delim <> Regex.replace tick ("\\" <> delim) ident <> delim
  where
  identifier = Regex.unsafeRegex "^[a-z][_a-z0-9]*$" Regex.ignoreCase
  tick = Regex.unsafeRegex delim Regex.global
