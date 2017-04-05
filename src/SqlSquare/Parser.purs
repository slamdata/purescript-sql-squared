module SqlSquare.Parser where

import Prelude

--import Data.Array as A
--import Data.Json.Extended as EJ
import Data.HugeNum as HN
--import Data.String as S

--import SqlSquare.Signature as Sig

import SqlSquare.Utils ((∘))

--import Matryoshka (class Corecursive)

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
--import Text.Parsing.Parser.Token as PT
--import Text.Parsing.Parser.Language as PL
--import Text.Parsing.Parser.Token as PT
import Text.Parsing.Parser.String as PS


data Literal
  = Null
  | String String
  | Boolean Boolean
  | Integer Int
  | Decimal HN.HugeNum

data Token
  = Kw String
  | Op String
  | Identifier String
  | Literal Literal


op ∷ ∀ m. Monad m ⇒ P.ParserT String m Token
op = map Op $ PC.choice
  [ PC.try $ PS.string "{*:}"
  , PC.try $ PS.string "{*}"
  , PC.try $ PS.string "{:*}"
  , PC.try $ PS.string "{_:}"
  , PC.try $ PS.string "{_}"
  , PC.try $ PS.string "[*:]"
  , PC.try $ PS.string "[*]"
  , PC.try $ PS.string "[:*]"
  , PC.try $ PS.string "[_]"
  , PC.try $ PS.string "..."
  , PC.try $ PS.string ".."
  , PC.try $ PS.string "<>"
  , PC.try $ PS.string "||"
  , PC.try $ PS.string "??"
  , PS.string "??"
  , PS.string "="
  , PS.string ">"
  , PS.string "<"
  , PS.string "["
  , PS.string "]"
  , PS.string ":"
  , PS.string ","
  , PS.string ";"
  ]

keywords ∷ Array String
keywords =
  [ "create"
  , "function"
  , "begin"
  , "end"
  , "import"
  , "select"
  , "distinct"
  , "delete"
  , "insert"
  , "into"
  , "values"
  , "as"
  , "or"
  , "and"
  , "is"
  , "not"
  , "between"
  , "in"
  , "like"
  , "escape"
  , "exists"
  , "when"
  , "then"
  , "else"
  , "case"
  , "null"
  , "true"
  , "false"
  , "from"
  , "join"
  , "on"
  , "cross"
  , "left"
  , "right"
  , "full"
  , "outer"
  , "inner"
  , "where"
  , "group"
  , "by"
  , "order"
  , "having"
  , "asc"
  , "desc"
  , "limit"
  , "offset"
  , "sample"
  , "union"
  , "all"
  , "intersect"
  , "except"
  ]


keyword ∷ ∀ m. Monad m ⇒ P.ParserT String m Token
keyword = map Kw $ PC.choice $ map parseKeyWord keywords

parseKeyWord ∷ ∀ m. Monad m ⇒ String → P.ParserT String m String
parseKeyWord = PC.try ∘ PS.string
