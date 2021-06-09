module SqlSquared.Signature.Ident
  ( Ident(..)
  , printIdent
  , genIdent
  ) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Set as Set
import Data.Show.Generic (genericShow)
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
  else "`" <> RX.replace slash ("\\\\") (RX.replace tick ("\\`") ident) <> "`"

identifier ∷ RX.Regex
identifier = RXU.unsafeRegex "^[a-z][_a-z0-9]*$" RXF.ignoreCase

tick ∷ RX.Regex
tick = RXU.unsafeRegex "`" RXF.global

slash ∷ RX.Regex
slash = RXU.unsafeRegex "\\\\" RXF.global

genIdent ∷ ∀ m. Gen.MonadGen m ⇒ m Ident
genIdent = do
  start ← Gen.elements $ "a" :| S.split (S.Pattern "") "bcdefghijklmnopqrstuvwxyz"
  body ← map (Int.toStringAs Int.hexadecimal) (Gen.chooseInt 0 100000)
  pure $ Ident (start <> body)
