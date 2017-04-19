module SqlSquare.Signature.Ident where

import Prelude

import Data.String.Regex (test, replace) as Regex
import Data.String.Regex.Flags (ignoreCase, global) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex

printIdent ∷ String → String
printIdent str =
  if Regex.test identifier str
    then str
    else "`" <> Regex.replace tick "\\`" str <> "`"
  where
  identifier = Regex.unsafeRegex "^[_a-z][_a-z0-9]*$" Regex.ignoreCase
  tick = Regex.unsafeRegex "`" Regex.global
