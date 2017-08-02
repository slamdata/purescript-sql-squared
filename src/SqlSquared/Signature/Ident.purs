module SqlSquared.Signature.Ident where

import Prelude

import Data.Set as Set
import Data.String as S
import Data.String.Regex (test, replace) as Regex
import Data.String.Regex.Flags (ignoreCase, global) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import SqlSquared.Parser.Tokenizer (keywords)

printIdent ∷ String → String
printIdent ident =
  if Regex.test identifier ident && not (Set.member (S.toLower ident) keywords)
    then ident
    else "`" <> Regex.replace tick ("\\`") ident <> "`"
  where
  identifier = Regex.unsafeRegex "^[a-z][_a-z0-9]*$" Regex.ignoreCase
  tick = Regex.unsafeRegex "`" Regex.global
