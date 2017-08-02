module SqlSquared.Signature.Ident where

import SqlSquared.Utils (escapeIdent)

printIdent ∷ String → String
printIdent = escapeIdent "`"
