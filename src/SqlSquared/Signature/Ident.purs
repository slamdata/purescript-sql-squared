module SqlSquared.Signature.Ident (Ident(..), printIdent) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Set as Set
import Data.String as S
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as RXU
import SqlSquared.Parser.Tokenizer (keywords)

newtype Ident = Ident String

derive newtype instance eqIdent :: Eq Ident
derive newtype instance ordIdent :: Ord Ident
derive instance newtypeIdent :: Newtype Ident _
derive instance genericIdent :: Generic Ident _
instance showIdent :: Show Ident where show = genericShow

printIdent ∷ Ident → String
printIdent (Ident ident) =
  if RX.test identifier ident && not (Set.member (S.toLower ident) keywords)
  then ident
  else "`" <> RX.replace tick ("\\`") ident <> "`"

identifier ∷ RX.Regex
identifier = RXU.unsafeRegex "^[a-z][_a-z0-9]*$" RXF.ignoreCase

tick ∷ RX.Regex
tick = RXU.unsafeRegex "`" RXF.global
