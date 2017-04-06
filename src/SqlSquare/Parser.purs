module SqlSquare.Parser
  ( module SqlSquare.Parser.Tokenizer
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (gets, put)
import Control.MonadPlus (guard)

import Data.Array as A
import Data.NonEmpty ((:|))
--import Data.Int as Int
--import Data.Either (Either)
import Data.List ((:))
import Data.List as L
import Data.Foldable as F
import Data.Maybe (Maybe(..), isNothing, fromMaybe, isJust)
import Data.Json.Extended as EJ
import Data.String as S
--import Data.Foldable as F
--import Data.HugeNum as HN

import SqlSquare.Utils ((∘), type (×), (×))
import SqlSquare.Parser.Tokenizer (Token(..), tokenize, Literal(..))
import SqlSquare.Signature as Sig
import Matryoshka (class Corecursive, embed)

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Pos (initialPos)
--import Text.Parsing.Parser.String as PS

import Unsafe.Coerce (unsafeCoerce)

token ∷ ∀ m. Monad m ⇒ P.ParserT (Array Token) m Token
token = do
  input ← gets \(P.ParseState input _ _) → input
  case A.uncons input of
    Nothing → P.fail "Unexpected EOF"
    Just {head, tail} → do
      put $ P.ParseState tail initialPos true
      pure head

whenTok ∷ ∀ m. Monad m ⇒ (Token → Boolean) → P.ParserT (Array Token) m Token
whenTok f = PC.try do
  a ← token
  guard $ f a
  pure a

ident ∷ ∀ m. Monad m ⇒ P.ParserT (Array Token) m String
ident = PC.try do
  a ← token
  case a of
    Identifier s → pure s
    _ → P.fail "Token is not an identifier"

operator ∷ ∀ m. Monad m ⇒ String →  P.ParserT (Array Token) m Unit
operator s = PC.try do
  a ← token
  case a of
    Op ss | s == ss → pure unit
    _ → P.fail $  "Token is not an operator " <> s

keyword ∷ ∀ m. Monad m ⇒ String →  P.ParserT (Array Token) m String
keyword s = PC.try do
  a ← token
  case a of
    Kw ss | S.toLower s == S.toLower ss → pure s
    _ → P.fail $ "Token is not a keyword " <> s

match ∷ ∀ m. Monad m ⇒ Token → P.ParserT (Array Token) m Token
match = whenTok ∘ eq

expr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
expr = letExpr

letExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
letExpr =
  (do i ← ident
      operator ":="
      bindTo ← expr
      operator ";"
      in_ ← expr
      pure $ embed $ Sig.Let { ident: i, bindTo, in_ } )
  <|> queryExpr

queryExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
queryExpr = do
  lhs ← (query <|> definedExpr)
  op ← queryBinop
  rhs ← (query <|> definedExpr)
  pure $ embed $ Sig.Binop { lhs, rhs, op }

queryBinop ∷ ∀ m. Monad m ⇒ P.ParserT (Array Token) m Sig.BinaryOperator
queryBinop =
  (keyword "limit" $> Sig.Limit)
  <|> (keyword "offset" $> Sig.Offset)
  <|> (keyword "sample" $> Sig.Sample)
  <|> (keyword "union" $> Sig.Union)
  <|> (keyword "union" *> keyword "all" $> Sig.UnionAll)
  <|> (keyword "intersect" $> Sig.Intersect)
  <|> (keyword "intersect" *> keyword "all" $> Sig.IntersectAll)
  <|> (keyword "except" $> Sig.Except)


-- TODO, add update and delete
query ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
query = selectExpr

definedExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
definedExpr = do
  lhs ← rangeExpr
  operator "??"
  rhs ← rangeExpr
  pure $ embed $ Sig.Binop { lhs, rhs, op: Sig.IfUndefined }

rangeExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
rangeExpr = do
  lhs ← orExpr
  operator ".."
  rhs ← orExpr
  pure $ embed $ Sig.Binop { lhs, rhs, op: Sig.Range }

orExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
orExpr = do
  lhs ← andExpr
  keyword "or"
  rhs ← andExpr
  pure $ embed $ Sig.Binop { lhs, rhs, op: Sig.Or }

andExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
andExpr = do
  lhs ← cmpExpr
  keyword "and"
  rhs ← cmpExpr
  pure $ embed $ Sig.Binop { lhs, rhs, op: Sig.And }

cmpExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
cmpExpr = do
  e ← defaultExpr
  modifiers ← A.many $ negatableSuffix <|> relationalSuffix
  pure $ F.foldl (\acc fn → fn acc) e modifiers

defaultExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
defaultExpr = do
  lhs ← concatExpr
  op ← searchLikeOp
  rhs ← concatExpr
  pure $ op lhs rhs

searchLikeOp
  ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (t → t → t)
searchLikeOp =
  (operator "~" $> (_SEARCH false))
  <|> (operator "~*" $> (_SEARCH true))
  <|> (operator "!~" $> (\a b → _NOT $ _SEARCH false a b))
  <|> (operator "!~*" $> (\a b → _NOT $ _SEARCH true a b))
  <|> (operator "~~" $> _LIKE Nothing)
  <|> (operator "!~~" $> (\a b → _NOT $ _LIKE Nothing a b))

concatExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
concatExpr = do
  lhs ← addExpr
  operator "||"
  rhs ← addExpr
  pure $ embed $ Sig.Binop {op: Sig.Concat, lhs, rhs}

addExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
addExpr = do
  lhs ← multExpr
  op ← (operator "+" $> Sig.Plus) <|> (operator "-" $> Sig.Minus)
  rhs ← multExpr
  pure $ embed $ Sig.Binop {op, lhs, rhs}

multExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
multExpr = do
  lhs ← powExpr
  op ← (operator "*" $> Sig.Mult) <|> (operator "/" $> Sig.Div) <|> (operator "%" $> Sig.Mod)
  rhs ← powExpr
  pure $ embed $ Sig.Binop {op, lhs, rhs}

powExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
powExpr = do
  lhs ← derefExpr
  operator "^"
  rhs ← derefExpr
  pure $ embed $ Sig.Binop {op: Sig.Pow, lhs, rhs}

derefExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
derefExpr = do
  e ← primaryExpression
  modifiers ← A.many $ operator "." *> modifier
  (mbWildcard ∷ Maybe t) ← PC.optionMaybe do
    operator "."
    wildcard
  let
    modified = F.foldl (\a f → f a) e modifiers
  pure case mbWildcard of
    Nothing → modified
    Just _ → embed $ Sig.Splice $ Just modified
  where
  modifier =
    (ident
     <#> (embed ∘ Sig.Literal ∘ EJ.String)
     <#> (\e → embed ∘ Sig.Binop ∘ { op: Sig.FieldDeref, lhs: e, rhs: _}))
    <|> (operator "{*:}"
         $> (\e → embed $ Sig.Unop { op: Sig.FlattenMapKeys, expr: e}))
    <|> ((operator "{*}" <|> operator "{:*}")
         $> (\e → embed $ Sig.Unop {op: Sig.FlattenMapValues, expr: e}))
    <|> (operator "{_:}"
         $> (\e → embed $ Sig.Unop { op: Sig.ShiftMapKeys, expr: e}))
    <|> ((operator "{_}" <|> operator "{:_}")
         $> (\e → embed $ Sig.Unop {op: Sig.ShiftMapValues, expr: e}))
    <|> (do operator "{"
            rhs ← expr
            operator "}"
            pure \e → embed $ Sig.Binop { op: Sig.FieldDeref, lhs: e, rhs })
    <|> (operator "[*:]"
         $> (\e → embed $ Sig.Unop { op: Sig.FlattenArrayIndices, expr: e}))
    <|> ((operator "[*]" <|> operator "[:*]")
         $> (\e → embed $ Sig.Unop { op: Sig.FlattenArrayValues, expr: e}))
    <|> (operator "[_:]"
         $> (\e → embed $ Sig.Unop { op: Sig.ShiftArrayIndices, expr: e}))
    <|> ((operator "[_]" <|> operator "[:_]")
         $> (\e → embed $ Sig.Unop { op: Sig.ShiftArrayValues, expr: e}))
    <|> (do operator "["
            rhs ← expr
            operator "]"
            pure \e → embed $ Sig.Binop { op: Sig.IndexDeref, lhs: e, rhs })

wildcard ∷ ∀ m t. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
wildcard = operator "*" $> (embed $ Sig.Splice Nothing)

primaryExpression
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m t
primaryExpression =
  caseExpr
  <|> unshiftExpr
  <|> (parenList <#> case _ of
          x : L.Nil → embed $ Sig.Parens x
          xs → embed $ Sig.SetLiteral xs )
  <|> unaryOperator
  <|> functionExpr
  <|> variable
  <|> literal
  <|> wildcard
  <|> arrayLiteral
  <|> mapLiteral
  <|> (ident <#> (embed ∘ Sig.Ident))

caseExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
caseExpr =
  (do keyword "case"
      (cs × else_) ← cases
      pure $ embed $ Sig.Switch { cases: cs, else_ })
  <|> (do keyword "case"
          e ← expr
          (cs × else_) ← cases
          pure $ embed $ Sig.Match { cases: cs, expr: e, else_ })

cases
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (L.List (Sig.Case t) × (Maybe t))
cases = do
  cs ←
    L.some do
      keyword "when"
      cond ← expr
      keyword "then"
      e ← expr
      pure $ Sig.Case { cond, expr: e}
  else_ ←
    PC.optionMaybe do
      keyword "else"
      expr
  keyword "end"
  pure $ cs × else_

unshiftExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
unshiftExpr =
  (do operator "["
      e ← expr
      operator "..."
      operator "]"
      pure $ embed $ Sig.Unop { op: Sig.UnshiftArray, expr: e })
  <|> (do operator "{"
          lhs ← expr
          operator ":"
          rhs ← expr
          operator "..."
          operator "}"
          pure $ embed $ Sig.Binop { op: Sig.UnshiftMap, lhs, rhs })

parenList
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (L.List t)
parenList = do
  operator "("
  arr ← PC.sepBy expr $ operator ","
  operator ")"
  pure $ L.fromFoldable arr

unaryOperator ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
unaryOperator = do
  op ←
    (operator "-" $> Sig.Negative)
    <|> (operator "+" $> Sig.Positive)
    <|> (keyword "distinct" $> Sig.Distinct)
    <|> (keyword "not" $> Sig.Distinct)
    <|> (keyword "exists" $> Sig.Distinct)
  e ← primaryExpression
  pure $ embed $ Sig.Unop { op, expr: e}

functionExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
functionExpr = do
  name ← ident
  args ← parenList
  pure $ embed $ Sig.InvokeFunction {name, args}

variable ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
variable = do
  operator ":"
  s ← ident
  pure $ embed $ Sig.Vari s

literal ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
literal = PC.try $ token >>= case _ of
  Lit (String s) → pure $ embed $ Sig.Literal $ EJ.String s
  Lit (Integer i) → pure $ embed $ Sig.Literal $ EJ.Integer i
  Lit (Decimal d) → pure $ embed $ Sig.Literal $ EJ.Decimal d
  Kw s | s == "null" → pure $ embed $ Sig.Literal $ EJ.Null
       | s == "true" → pure $ embed $ Sig.Literal $ EJ.Boolean true
       | s == "false" → pure $ embed $ Sig.Literal $ EJ.Boolean false
  _ → P.fail "incorrect literal"

arrayLiteral ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
arrayLiteral = do
  operator "["
  els ← PC.sepBy expr $ operator ","
  operator "]"
  pure $ embed $ Sig.Literal $ EJ.Array $ A.fromFoldable els

mapLiteral ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
mapLiteral = do
  operator "{"
  els ← PC.sepBy pair $ operator ","
  operator "}"
  pure $ embed $ Sig.Literal $ EJ.Map $ EJ.EJsonMap $ A.fromFoldable els

pair ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m (t × t)
pair = do
  l ← expr
  operator ":"
  r ← expr
  pure $ l × r

negatableSuffix
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (t → t)
negatableSuffix = do
  mbInv ← PC.optionMaybe do
    keyword "is"
    n ← PC.optionMaybe $ keyword "not"
    pure $ isNothing  n
  let inv = fromMaybe true mbInv
  suffix ← betweenSuffix <|> inSuffix <|> likeSuffix
  pure $ \e → (if inv then _NOT else id) $ suffix e

betweenSuffix
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (t → t)
betweenSuffix = do
  keyword "between"
  lhs ← defaultExpr
  keyword "and"
  rhs ← defaultExpr
  pure $ \e → embed $ Sig.InvokeFunction {name: "BETWEEN", args: e:lhs:rhs:L.Nil }

inSuffix
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (t → t)
inSuffix = do
  keyword "in"
  rhs ← defaultExpr
  pure $ \lhs → embed $ Sig.Binop { op: Sig.In, lhs, rhs }

likeSuffix
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (t → t)
likeSuffix = do
  keyword "like"
  rhs ← defaultExpr
  mbEsc ← PC.optionMaybe do
    keyword "escape"
    defaultExpr
  pure $ \lhs → _LIKE mbEsc lhs rhs


relationalSuffix
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (t → t)
relationalSuffix = do
  op ← relationalOp
  rhs ← defaultExpr
  pure $ \lhs → embed $ Sig.Binop { op, lhs, rhs }

relationalOp ∷ ∀ m. Monad m ⇒ P.ParserT (Array Token) m (Sig.BinaryOperator)
relationalOp =
  (operator "=" $> Sig.Eq)
  <|> (operator "<>" $> Sig.Neq)
  <|> (operator "!=" $> Sig.Neq)
  <|> (operator "<" $> Sig.Lt)
  <|> (operator "<=" $> Sig.Le)
  <|> (operator ">" $> Sig.Gt)
  <|> (operator ">=" $> Sig.Ge)
  <|> (PC.try (keyword "is" *> keyword "not") $> Sig.Neq)
  <|> (keyword "is" $> Sig.Eq)

selectExpr ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
selectExpr = do
  keyword "select"
  isDistinct ← map isJust $ PC.optionMaybe $ keyword "distinct"
  prs ← projections
  rels ← PC.optionMaybe relations
  fil ← PC.optionMaybe filter
  gb ← PC.optionMaybe groupBy
  ob ← PC.optionMaybe orderBy
  pure
    $ embed
    $ Sig.Select
    { isDistinct: false
    , projections: prs
    , relations: rels
    , filter: fil
    , groupBy: gb
    , orderBy: ob
    }

relations
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (Sig.Relation t)
relations = do
  keyword "from"
  rels ← PC.sepBy1 relation $ operator ","
  let
    foldFn Nothing rel = Just rel
    foldFn (Just left) right =
      Just $ Sig.JoinRelation
        { joinType: Sig.InnerJoin, left, right
        , clause: embed $ Sig.Literal $ EJ.Boolean true
        }
    res = F.foldl foldFn Nothing rels
  case res of
    Just a → pure a
    Nothing → P.fail "incorrect relations"

relation
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (Sig.Relation t)
relation = do
  first ← simpleRelation
  modifiers ← A.many (stdJoinRelation <|> crossJoinRelation)
  pure $ F.foldl (\a f → f a) first modifiers

simpleRelation
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (Sig.Relation t)
simpleRelation = unsafeCoerce unit

stdJoinRelation
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (Sig.Relation t → Sig.Relation t)
stdJoinRelation = do
  joinType ←
    (((Sig.LeftJoin <$ keyword "left")
     <|> (Sig.RightJoin <$ keyword "right")
     <|> (Sig.FullJoin <$ keyword "full"))
     <* (PC.optional $ keyword "outer"))
    <|> (Sig.InnerJoin <$ keyword "inner")
  keyword "join"
  right ← simpleRelation
  keyword "on"
  clause ← expr
  pure \left →
    Sig.JoinRelation
      { left
      , right
      , joinType
      , clause
      }
crossJoinRelation
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (Sig.Relation t → Sig.Relation t)
crossJoinRelation = do
  keyword "cross"
  keyword "join"
  right ← simpleRelation
  pure \left →
    Sig.JoinRelation
      { joinType: Sig.InnerJoin
      , left
      , right
      , clause: embed $ Sig.Literal $ EJ.Boolean true
      }


filter ∷ ∀ t m. (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m) ⇒ P.ParserT (Array Token) m t
filter = do
  keyword "where"
  definedExpr

groupBy
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (Sig.GroupBy t)
groupBy = do
  keyword "group"
  keyword "by"
  keys ← PC.sepBy1 definedExpr $ operator ","
  having ← PC.optionMaybe do
    keyword "having"
    definedExpr
  pure $ Sig.GroupBy { keys, having }


orderBy
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (Sig.OrderBy t)
orderBy = do
  keyword "order"
  keyword "by"
  lst ← flip PC.sepBy1 (operator ",") do
    v ← definedExpr
    sortStr ← map (fromMaybe "asc") $ PC.optionMaybe (keyword "asc" <|> keyword "desc")
    sort ←
      case sortStr of
        "asc" → pure Sig.ASC
        "desc" → pure Sig.DESC
        _ → P.fail "incorrect sort"
    pure $ sort × v
  case lst of
    L.Nil → P.fail "incorrect order by"
    x : xs → pure $ Sig.OrderBy (x :| xs)

projections
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (L.List (Sig.Projection t))
projections = PC.sepBy projection $ operator ","

projection
  ∷ ∀ t m
  . (Corecursive t (Sig.SqlF EJ.EJsonF), Monad m)
  ⇒ P.ParserT (Array Token) m (Sig.Projection t)
projection = do
  e ← definedExpr
  a ← PC.optionMaybe do
    keyword "as"
    ident
  pure $ Sig.Projection { expr: e, alias: a}


_SEARCH ∷ ∀ t.Corecursive t (Sig.SqlF EJ.EJsonF) ⇒ Boolean → t → t → t
_SEARCH b lhs rhs =
  embed
  $ Sig.InvokeFunction
    { name: "SEARCH"
    , args:
      lhs
      : rhs
      : (embed $ Sig.Literal $ EJ.Boolean b)
      : L.Nil
    }

_LIKE ∷ ∀ t.Corecursive t (Sig.SqlF EJ.EJsonF) ⇒ Maybe t → t → t → t
_LIKE mbEsc lhs rhs =
  embed
  $ Sig.InvokeFunction
    { name: "LIKE"
    , args:
       lhs
       : rhs
       : (fromMaybe (embed $ Sig.Literal $ EJ.String "\\") mbEsc)
       : L.Nil
    }


_NOT ∷ ∀ t. Corecursive t (Sig.SqlF EJ.EJsonF) ⇒ t → t
_NOT e = embed $ Sig.Unop {op: Sig.Not, expr: e}
