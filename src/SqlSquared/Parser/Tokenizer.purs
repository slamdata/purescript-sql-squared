module SqlSquared.Parser.Tokenizer
  ( tokenize
  , Token(..)
  , Literal(..)
  , PositionedToken
  , TokenStream
  , printToken
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (get)
import Data.Array as A
import Data.Char as Ch
import Data.Either (Either)
import Data.HugeInt as HI
import Data.HugeNum as HN
import Data.Json.Extended.Signature.Parse as EJP
import Data.Maybe (isJust)
import Data.String as S
import SqlSquared.Utils ((∘))
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Pos as PP
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT

data Literal
  = String String
  | Integer HI.HugeInt
  | Decimal HN.HugeNum

derive instance eqTokenLit ∷ Eq Literal

data Token
  = Kw String
  | Op String
  | Identifier String
  | Lit Literal
  | Comment String

derive instance eqToken ∷ Eq Token

type PositionedToken =
  { position ∷ PP.Position
  , token ∷ Token
  }

type TokenStream = Array PositionedToken

isKeyword ∷ Token → Boolean
isKeyword = case _ of
  Kw _ → true
  _ → false

isComment ∷ Token → Boolean
isComment = case _ of
  Comment _ → true
  _ → false

printToken ∷ Token → String
printToken = case _ of
  Kw str → "keyword `" <> S.toUpper str <> "`"
  Op str → "`" <> str <> "`"
  Identifier _ → "identifier"
  Lit (String _) → "string literal"
  Lit (Integer _) → "integer literal"
  Lit (Decimal _) → "decimal literal"
  Comment str → "comment"

op ∷ ∀ m. Monad m ⇒ P.ParserT String m Token
op = map Op $ PC.choice $ map PS.string operators

operators ∷ Array String
operators =
  [ "{*:}"
  , "{*}"
  , "{:*}"
  , "{_:}"
  , "{_}"
  , "[*:]"
  , "[*]"
  , "[:*]"
  , "[_]"
  , "..."
  , ".."
  , "<>"
  , "!="
  , "||"
  , "??"
  , "!~*"
  , "!~"
  , "!~~"
  , "~~"
  , "~*"
  , ":="
  , "~"
  , "??"
  , "="
  , ">"
  , "<"
  , "["
  , "]"
  , ":"
  , ","
  , ";"
  , "*"
  , "("
  , ")"
  , "{"
  , "}"
  , "-"
  , "+"
  , "^"
  , "."
  , "/"
  , "%"
  ]

keywords ∷ Array String
keywords =
  [ "where"
  , "when"
  , "values"
  , "union"
  , "true"
  , "then"
  , "select"
  , "sample"
  , "right"
  , "outer"
  , "order"
  , "or"
  , "on"
  , "offset"
  , "null"
  , "not"
  , "limit"
  , "like"
  , "left"
  , "join"
  , "is"
  , "into"
  , "intersect"
  , "insert"
  , "inner"
  , "in"
  , "import"
  , "having"
  , "group"
  , "function"
  , "full"
  , "from"
  , "false"
  , "exists"
  , "except"
  , "escape"
  , "end"
  , "else"
  , "distinct"
  , "desc"
  , "delete"
  , "cross"
  , "create"
  , "case"
  , "by"
  , "between"
  , "begin"
  , "asc"
  , "as"
  , "and"
  , "all"
  ]

digits ∷ Array Char
digits = ['0','1','2','3','4','5','6','7','8','9' ]

ident ∷ ∀ m. Monad m ⇒ P.ParserT String m Token
ident = map Identifier $ PC.try quotedIdent <|> PC.try notQuotedIdent

oneLineComment ∷ ∀ m. Monad m ⇒ P.ParserT String m Token
oneLineComment =
  PC.between (PC.try $ PS.string "--") (PS.string "\n")
    $ map (Comment ∘ S.fromCharArray)
    $ A.many $ PS.satisfy
    $ not ∘ eq '\n'

multiLineComment ∷ ∀ m. Monad m ⇒ P.ParserT String m Token
multiLineComment = do
  _ ← PS.string "/*"
  m ← collectBeforeComment ""
  pure $ Comment m
  where
  collectBeforeComment acc =
    let l = S.length acc
    in do
      c ← PS.anyChar
      case S.drop (l - 1) acc of
        "*" | c == '/' →
          pure acc
        _ →
          collectBeforeComment $ acc <> S.fromCharArray [ c ]

quotedIdent ∷ ∀ m. Monad m ⇒ P.ParserT String m String
quotedIdent =
  PC.between (PS.string "`") (PS.string "`")
    $ map S.fromCharArray
    $ A.some identChar
  where
  identChar = identEscape <|> identLetter
  identLetter = PS.satisfy (not ∘ eq '`')
  identEscape = PS.string "\\`" $> '`'

notQuotedIdent ∷ ∀ m. Monad m ⇒ P.ParserT String m String
notQuotedIdent = do
  first ← PT.letter
  other ← A.many (PT.alphaNum <|> PS.char '_')
  let
    str = S.fromCharArray $ A.cons first other
  if isJust $ A.elemIndex (S.toLower str) keywords
    then P.fail "unexpected keyword"
    else pure str

stringLit ∷ ∀ m. Monad m ⇒ P.ParserT String m Token
stringLit = Lit ∘ String <$> EJP.parseStringLiteral

numLit ∷ ∀ m. Monad m ⇒ P.ParserT String m Token
numLit = Lit ∘ Decimal <$> EJP.parseDecimalLiteral

intLit ∷ ∀ m. Monad m ⇒ P.ParserT String m Token
intLit = Lit ∘ Integer <$> EJP.parseHugeIntLiteral

keyword ∷ ∀ m. Monad m ⇒ P.ParserT String m Token
keyword = map Kw $ PC.choice $ map (PC.try ∘ parseKeyword) keywords

parseKeyword ∷ ∀ m. Monad m ⇒ String → P.ParserT String m String
parseKeyword s =
  map S.fromCharArray $ A.foldM foldFn [ ] $ S.toCharArray s
  where
  foldFn acc ch = do
    c ← PC.try $ PS.oneOf [ Ch.toUpper ch, Ch.toLower ch ]
    pure $ A.snoc acc c

positioned ∷ ∀ m. Monad m ⇒ P.ParserT String m Token → P.ParserT String m PositionedToken
positioned m = do
  P.ParseState _ position _ ← get
  { position, token: _ } <$> m

tokens ∷ ∀ m. Monad m ⇒ P.ParserT String m TokenStream
tokens = do
  PS.skipSpaces
  A.some $ positioned $ PC.choice
    [ skipped oneLineComment
    , skipped multiLineComment
    , skipped op
    , skipped keyword
    , skipped ident
    , skipped numLit
    , skipped intLit
    , skipped stringLit
    ]
  where
  skipped r = PC.try (r <* PS.skipSpaces)

tokenize ∷ String → Either P.ParseError TokenStream
tokenize input =
  A.filter (not ∘ isComment ∘ _.token) <$>  P.runParser input tokens
