module SqlSquared.Parser
  ( parse
  , parseQuery
  , parseModule
  , prettyParse
  , module SqlSquared.Parser.Tokenizer
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.Error.Class (catchError)
import Control.Monad.State (get, put)
import Control.MonadZero (guard)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either as E
import Data.Foldable as F
import Data.Json.Extended as EJ
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.NonEmpty ((:|))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), uncurry)
import Matryoshka (class Corecursive, embed)
import SqlSquared.Constructors as C
import SqlSquared.Parser.Tokenizer (Token(..), TokenStream, PositionedToken, tokenize, Literal(..), printToken)
import SqlSquared.Path as Pt
import SqlSquared.Signature as Sig
import SqlSquared.Signature.Ident (Ident(..))
import SqlSquared.Utils ((∘), type (×), (×))
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Pos as PP

type SqlParser m t r =
  Corecursive t (Sig.SqlF EJ.EJsonF)
  ⇒ Monad m
  ⇒ P.ParserT TokenStream m r

type SqlParser' m t = SqlParser m t t

withErrorMessage ∷ ∀ m a. Monad m ⇒ (String → String) → P.ParserT TokenStream m a → P.ParserT TokenStream m a
withErrorMessage k = withErrorMessage' (append "Expected " ∘ k)

withErrorMessage' ∷ ∀ m a. Monad m ⇒ (String → String) → P.ParserT TokenStream m a → P.ParserT TokenStream m a
withErrorMessage' k p = catchError p \parseError →
  P.failWithPosition
    (k (P.parseErrorMessage parseError))
    (P.parseErrorPosition parseError)

asErrorMessage ∷ ∀ m a. Monad m ⇒ String → P.ParserT TokenStream m a → P.ParserT TokenStream m a
asErrorMessage err = flip (<|>) do
  P.ParseState input _ _ ← get
  case A.head input of
    Nothing → P.fail $ "Expected " <> err <> ", but got end of input"
    Just tk → P.failWithPosition ("Expected " <> err <> ", but got " <> printToken tk.token) tk.position

withToken ∷ ∀ m a. Monad m ⇒ String → (Token → P.ParserT TokenStream m a) → P.ParserT TokenStream m a
withToken err k =
  PC.try
    $ withErrorMessage (append $ err <> ", but got ")
    $ (withErrorMessage' (const "end of input") token) >>= k

prettyParse
  ∷ ∀ a
  . (String → E.Either P.ParseError a)
  → String
  → E.Either String a
prettyParse parser input =
  lmap printError (parser input)
  where
  padLeft n s =
    SCU.fromCharArray (A.replicate (n - S.length s) ' ') <> s

  printError parseError =
    let
      message = P.parseErrorMessage parseError
      PP.Position pos = P.parseErrorPosition parseError
      lines = S.split (S.Pattern "\n") input
      pre = A.drop (pos.line - 3) $ A.take (pos.line - 1) lines
      line = A.take 1 $ A.drop (pos.line - 1) lines
      post = A.take 3 $ A.drop pos.line lines
      nums = A.mapWithIndex (\n l → padLeft 4 (show (n + pos.line - (A.length pre))) <> " | " <> l) (pre <> line <> post)
      pointer = pure $ SCU.fromCharArray (A.replicate (pos.column - 1 + 7) '-') <> "^ " <> message
    in
      S.joinWith "\n" $ A.take (A.length pre + 1) nums <> pointer <> A.drop 3 nums

parse
  ∷ ∀ t
  . Corecursive t (Sig.SqlF EJ.EJsonF)
  ⇒ String
  → E.Either P.ParseError t
parse = tokenize >=> flip P.runParser (expr <* eof)

parseQuery
  ∷ ∀ t
  . Corecursive t (Sig.SqlF EJ.EJsonF)
  ⇒ String
  → E.Either P.ParseError (Sig.SqlQueryF t)
parseQuery = tokenize >=> flip P.runParser (queryTop <* eof)

parseModule
  ∷ ∀ t
  . Corecursive t (Sig.SqlF EJ.EJsonF)
  ⇒ String
  → E.Either P.ParseError (Sig.SqlModuleF t)
parseModule = tokenize >=> flip P.runParser (moduleTop <* eof)

queryTop ∷ ∀ m t. SqlParser m t (Sig.SqlQueryF t)
queryTop = defer \_ → Sig.Query <$> (PC.sepEndBy decl $ operator ";") <*> expr

moduleTop ∷ ∀ m t. SqlParser m t (Sig.SqlModuleF t)
moduleTop = defer \_ → Sig.Module <$> PC.sepBy decl (operator ";")

decl ∷ ∀ m t. SqlParser m t (Sig.SqlDeclF t)
decl = asErrorMessage "import or function declaration" do
  import_ <|> functionDecl expr

token ∷ ∀ m. Monad m ⇒ P.ParserT TokenStream m Token
token = do
  P.ParseState input _ _ ← get
  case A.uncons input of
    Nothing → P.fail "Unexpected end of input"
    Just { head, tail } → do
      put $ P.ParseState tail head.position true
      pure head.token

eof ∷ ∀ m. Monad m ⇒ P.ParserT TokenStream m Unit
eof = do
  P.ParseState input _ _ ← get
  case A.head input of
    Nothing → pure unit
    Just tk → P.failWithPosition ("Unexpected " <> printToken tk.token) tk.position

prod
  ∷ ∀ a b m i
  . Monad m
  ⇒ P.ParserT i m a
  → P.ParserT i m b
  → (a → b → a → a)
  → P.ParserT i m a
prod pa pb f = do
  l ← pa
  p ← A.many $ Tuple <$> pb <*> pa
  pure $ F.foldl (uncurry ∘ f) l p

ident ∷ ∀ m. Monad m ⇒ P.ParserT TokenStream m String
ident = withToken "identifier" case _ of
  Identifier s → pure s
  tok → P.fail $ printToken tok

operator ∷ ∀ m. Monad m ⇒ String →  P.ParserT TokenStream m Unit
operator s = withToken (printToken (Op s)) case _ of
  Op ss | s == ss → pure unit
  tok → P.fail $ printToken tok

keyword ∷ ∀ m. Monad m ⇒ String →  P.ParserT TokenStream m String
keyword s = withToken (printToken (Kw s)) case _ of
  Kw ss | S.toLower s == S.toLower ss → pure s
  tok → P.fail $ printToken tok

anyKeyword ∷ ∀ m. Monad m ⇒ P.ParserT TokenStream m String
anyKeyword = withToken "keyword" case _ of
  Kw s → pure s
  tok → P.fail $ printToken tok

expr ∷ ∀ m t. SqlParser' m t
expr = asErrorMessage "let binding or expression" do
  letExpr <|> queryExpr

letExpr ∷ ∀ m t. SqlParser' m t
letExpr = do
  i ← PC.try (ident <* operator ":=")
  bindTo ← expr
  operator ";"
  in_ ← expr
  pure $ C.let_ (Ident i) bindTo in_

queryExpr ∷ ∀ m t. SqlParser' m t
queryExpr = prod (query <|> definedExpr) queryBinop _BINOP

queryBinop ∷ ∀ m. Monad m ⇒ P.ParserT TokenStream m Sig.BinaryOperator
queryBinop = asErrorMessage "query operator" $ PC.choice
  [ keyword "limit" $> Sig.Limit
  , keyword "offset" $> Sig.Offset
  , keyword "sample" $> Sig.Sample
  , PC.try $ keyword "union" *> keyword "all" $> Sig.UnionAll
  , keyword "union" $> Sig.Union
  , PC.try $ keyword "intersect" *> keyword "all" $> Sig.IntersectAll
  , keyword "intersect" $> Sig.Intersect
  , keyword "except" $> Sig.Except
  ]

-- TODO, add update and delete
query ∷ ∀ m t. SqlParser' m t
query = selectExpr

definedExpr ∷ ∀ m t. SqlParser' m t
definedExpr = prod rangeExpr (operator "??") $ _BINOP' Sig.IfUndefined

rangeExpr ∷ ∀ m t. SqlParser' m t
rangeExpr = prod orExpr (operator "..") $ _BINOP' Sig.Range

orExpr ∷ ∀ m t. SqlParser' m t
orExpr = prod andExpr (keyword "or") $ _BINOP' Sig.Or

andExpr ∷ ∀ m t. SqlParser' m t
andExpr = prod cmpExpr (keyword "and") $ _BINOP' Sig.And

cmpExpr ∷ ∀ m t. SqlParser' m t
cmpExpr = do
  e ← defaultExpr
  modifiers ← A.many $ PC.try negatableSuffix <|> relationalSuffix
  pure $ F.foldl (\acc fn → fn acc) e modifiers

defaultExpr ∷ ∀ m t. SqlParser' m t
defaultExpr = prod concatExpr searchLikeOp \lhs op rhs → op lhs rhs

searchLikeOp ∷ ∀ m t. SqlParser m t (t → t → t)
searchLikeOp = asErrorMessage "search operator" $ PC.choice
  [ operator "~" $> _SEARCH false
  , operator "~*" $> _SEARCH true
  , operator "!~" $> \a b → _NOT $ _SEARCH false a b
  , operator "!~*" $> \a b → _NOT $ _SEARCH true a b
  , operator "~~" $> _LIKE Nothing
  , operator "!~~" $> \a b → _NOT $ _LIKE Nothing a b
  ]

concatExpr ∷ ∀ m t. SqlParser' m t
concatExpr = _BINOP' Sig.Concat # prod addExpr (operator "||")

addExpr ∷ ∀ m t. SqlParser' m t
addExpr = prod multExpr addOp _BINOP
  where
  addOp = PC.choice
    [ operator "+" $> Sig.Plus
    , operator "-" $> Sig.Minus
    ]

multExpr ∷ ∀ m t. SqlParser' m t
multExpr = prod powExpr multOp _BINOP
  where
  multOp = PC.choice
    [ operator "*" $> Sig.Mult
    , operator "/" $> Sig.Div
    , operator "%" $> Sig.Mod
    ]

powExpr ∷ ∀ m t. SqlParser' m t
powExpr = prod derefExpr (operator "^") $ _BINOP' Sig.Pow

derefExpr ∷ ∀ m t. SqlParser' m t
derefExpr = do
  modified ← F.foldl (#) <$> primaryExpr <*> A.many modifier
  PC.optionMaybe (operator "." *> wildcard) <#> case _ of
    Nothing → modified
    Just (_ ∷ t) → embed $ Sig.Splice $ Just modified
  where
  modifier = asErrorMessage "dereference operator" $ PC.choice
    [ fieldDeref
    , operator "{*:}" $> C.unop Sig.FlattenMapKeys
    , operator "{*}" $> C.unop Sig.FlattenMapValues
    , operator "{:*}" $> C.unop Sig.FlattenMapValues
    , operator "{_:}" $> C.unop Sig.ShiftMapKeys
    , operator "{_}" $> C.unop Sig.ShiftMapValues
    , operator "{:_}" $> C.unop Sig.ShiftMapValues
    , fieldDerefExpr
    , operator "[*:]" $> C.unop Sig.FlattenArrayIndices
    , operator "[*]" $> C.unop Sig.FlattenArrayValues
    , operator "[:*]" $> C.unop Sig.FlattenArrayValues
    , operator "[_:]" $> C.unop Sig.ShiftArrayIndices
    , operator "[_]" $> C.unop Sig.ShiftArrayValues
    , indexDerefExpr
    ]

  fieldDeref = PC.try do
    operator "."
    k ← ident <|> anyKeyword <|> stringLiteral
    pure \e → C.binop Sig.FieldDeref e (C.ident k)

  fieldDerefExpr = do
    operator "{"
    rhs ← expr
    operator "}"
    pure \e → C.binop Sig.FieldDeref e rhs

  indexDerefExpr = do
    operator "["
    rhs ← expr
    operator "]"
    pure \e → C.binop Sig.IndexDeref e rhs

wildcard ∷ ∀ m t. SqlParser' m t
wildcard = operator "*" $> embed (Sig.Splice Nothing)

primaryExpr ∷ ∀ m t. SqlParser' m t
primaryExpr = asErrorMessage "primary expression" $ PC.choice
  [ PC.try caseExpr
  , PC.try unshiftExpr
  , parenList <#> case _ of
      x : L.Nil → embed $ Sig.Parens x
      xs → embed $ Sig.SetLiteral xs
  , unaryOperator
  , functionExpr
  , variable
  , literal
  , wildcard
  , arrayLiteral
  , mapLiteral
  , ident <#> embed ∘ Sig.Identifier ∘ Ident
  ]

caseExpr ∷ ∀ m t. SqlParser' m t
caseExpr = PC.try switchExpr <|> matchExpr
  where
  switchExpr = do
    _ ← keyword "case"
    uncurry C.switch <$> cases

  matchExpr = do
    _ ← keyword "case"
    e ← expr
    uncurry (C.match e) <$> cases

cases ∷ ∀ m t. SqlParser m t (L.List (Sig.Case t) × Maybe t)
cases = do
  cs ←
    L.some do
      _ ← keyword "when"
      cond ← expr
      _ ← keyword "then"
      e ← expr
      pure $ Sig.Case { cond, expr: e}
  else_ ←
    PC.optionMaybe do
      _ ← keyword "else"
      expr
  _ ← keyword "end"
  pure $ cs × else_

unshiftExpr ∷ ∀ m t. SqlParser' m t
unshiftExpr = unshiftArrayExpr <|> unshiftMapExpr
  where
  unshiftArrayExpr = do
    _ ← operator "["
    e ← expr
    _ ← operator "..."
    _ ← operator "]"
    pure $ C.unop Sig.UnshiftArray e

  unshiftMapExpr = do
    _ ← operator "{"
    lhs ← expr
    _ ← operator ":"
    rhs ← expr
    _ ← operator "..."
    _ ← operator "}"
    pure $ C.binop Sig.UnshiftMap lhs rhs

parenList ∷ ∀ m t. SqlParser m t (L.List t)
parenList = do
  operator "("
  arr ← PC.sepBy expr $ operator ","
  operator ")"
  pure $ L.fromFoldable arr

unaryOperator ∷ ∀ m t. SqlParser' m t
unaryOperator = do
  op ← unaryOp
  e ← primaryExpr
  pure $ C.unop op e
  where
  unaryOp = PC.choice
    [ operator "-" $> Sig.Negative
    , operator "+" $> Sig.Positive
    , keyword "distinct" $> Sig.Distinct
    , keyword "not" $> Sig.Not
    , keyword "exists" $> Sig.Exists
    ]

functionExpr ∷ ∀ m t. SqlParser' m t
functionExpr = PC.try do
  name ← ident <|> anyKeyword
  args ← parenList
  pure $ C.invokeFunction (Ident (S.toUpper name)) args

functionDecl
  ∷ ∀ m a
  . Monad m
  ⇒ P.ParserT TokenStream m a
  → P.ParserT TokenStream m (Sig.SqlDeclF a)
functionDecl parseExpr = asErrorMessage "function declaration" do
  _ ← PC.try $ keyword "create" *> keyword "function"
  name ← ident
  operator "("
  args ← PC.sepBy variableString $ operator ","
  operator ")"
  _ ← keyword "begin"
  body ← parseExpr
  _ ← keyword "end"
  pure $ Sig.FunctionDecl { ident: Ident name, args, body }

import_
  ∷ ∀ m a
  . Monad m
  ⇒ P.ParserT TokenStream m (Sig.SqlDeclF a)
import_ = asErrorMessage "import declaration" do
  _ ← keyword "import"
  s ← ident
  path ← Pt.parseAnyDirPath P.fail s
  pure $ Sig.Import path

variable ∷ ∀ m t. SqlParser' m t
variable = C.vari <$> variableString

variableString ∷ ∀ m. Monad m ⇒ P.ParserT TokenStream m Ident
variableString = asErrorMessage "variable" $ PC.try do
  operator ":"
  PP.Position pos1 ← P.position
  s ← ident <|> anyKeyword
  PP.Position pos2 ← P.position
  guard (pos1.line == pos2.line && pos2.column == pos1.column + 1)
  pure (Ident s)

literal ∷ ∀ m t. SqlParser' m t
literal = withToken "literal" case _ of
  Lit (String s) → pure $ embed $ Sig.Literal $ EJ.String s
  Lit (Integer i) → pure $ embed $ Sig.Literal $ EJ.Integer i
  Lit (Decimal d) → pure $ embed $ Sig.Literal $ EJ.Decimal d
  Kw s
    | S.toLower s == "null" → pure $ embed $ Sig.Literal $ EJ.Null
    | S.toLower s == "true" → pure $ embed $ Sig.Literal $ EJ.Boolean true
    | S.toLower s == "false" → pure $ embed $ Sig.Literal $ EJ.Boolean false
  t → P.fail (printToken t)

stringLiteral ∷ ∀ m. Monad m ⇒ P.ParserT TokenStream m String
stringLiteral = withToken "string literal" case _ of
  Lit (String s) → pure s
  t → P.fail (printToken t)

arrayLiteral ∷ ∀ m t. SqlParser' m t
arrayLiteral = do
  operator "["
  els ← PC.sepBy expr $ operator ","
  operator "]"
  pure $ embed $ Sig.Literal $ EJ.Array $ A.fromFoldable els

mapLiteral ∷ ∀ m t. SqlParser' m t
mapLiteral = do
  operator "{"
  els ← PC.sepBy keyValuePair $ operator ","
  operator "}"
  pure $ embed $ Sig.Literal $ EJ.Map $ EJ.EJsonMap $ A.fromFoldable els

keyValuePair ∷ ∀ m t. SqlParser m t (t × t)
keyValuePair = do
  l ← expr
  operator ":"
  r ← expr
  pure $ l × r

negatableSuffix ∷ ∀ m t. SqlParser m t (t → t)
negatableSuffix = do
  inv ← do
    _ ← PC.optionMaybe (keyword "is")
    n ← PC.optionMaybe $ keyword "not"
    pure $ isJust n
  suffix ← betweenSuffix <|> inSuffix <|> likeSuffix
  pure \e → (if inv then _NOT else identity) $ suffix e

betweenSuffix ∷ ∀ m t. SqlParser m t (t → t)
betweenSuffix = do
  _ ← keyword "between"
  lhs ← defaultExpr
  _ ← keyword "and"
  rhs ← defaultExpr
  pure \e → C.invokeFunction (Ident "BETWEEN") (e : lhs : rhs : L.Nil)

inSuffix ∷ ∀ m t. SqlParser m t (t → t)
inSuffix = do
  _ ← keyword "in"
  rhs ← defaultExpr
  pure \lhs → C.binop Sig.In lhs rhs

likeSuffix ∷ ∀ m t. SqlParser m t (t → t)
likeSuffix = do
  _ ← keyword "like"
  rhs ← defaultExpr
  mbEsc ← PC.optionMaybe do
    _ ← keyword "escape"
    defaultExpr
  pure \lhs → _LIKE mbEsc lhs rhs

relationalSuffix ∷ ∀ m t. SqlParser m t (t → t)
relationalSuffix = do
  op ← relationalOp
  rhs ← defaultExpr
  pure \lhs → C.binop op lhs rhs

relationalOp ∷ ∀ m. Monad m ⇒ P.ParserT TokenStream m (Sig.BinaryOperator)
relationalOp = PC.choice
  [ operator "=" $> Sig.Eq
  , operator "<>" $> Sig.Neq
  , operator "!=" $> Sig.Neq
  , operator "<" $> Sig.Lt
  , operator "<=" $> Sig.Le
  , operator ">" $> Sig.Gt
  , operator ">=" $> Sig.Ge
  , PC.try (keyword "is" *> keyword "not") $> Sig.Neq
  , keyword "is" $> Sig.Eq
  ]

selectExpr ∷ ∀ m t. SqlParser' m t
selectExpr = do
  _ ← keyword "select"
  isDistinct ← map isJust $ PC.optionMaybe $ keyword "distinct"
  prs ← projections
  rels ← PC.optionMaybe relations
  fil ← PC.optionMaybe filter
  gb ← PC.optionMaybe groupBy
  ob ← PC.optionMaybe $ orderBy prs
  pure $ embed $ Sig.Select
    { isDistinct
    , projections: prs
    , relations: rels
    , filter: fil
    , groupBy: gb
    , orderBy: ob
    }

relations ∷ ∀ m t. SqlParser m t (Sig.Relation t)
relations = do
  _ ← keyword "from"
  rels ← PC.sepBy1 relation $ operator ","
  let
    foldFn Nothing rel = Just rel
    foldFn (Just left) right =
      Just $ Sig.JoinRelation
        { joinType: Sig.InnerJoin, left, right
        , clause: C.bool true
        }
    res = F.foldl foldFn Nothing rels
  case res of
    Just a → pure a
    Nothing → P.fail "incorrect relations"

relation ∷ ∀ m t. SqlParser m t (Sig.Relation t)
relation = do
  first ← simpleRelation
  modifiers ← A.many (stdJoinRelation <|> crossJoinRelation)
  pure $ F.foldl (\a f → f a) first modifiers

simpleRelation ∷ ∀ m t. SqlParser m t (Sig.Relation t)
simpleRelation =
  tableRelation
  <|> variRelation
  <|> PC.try exprRelation
  <|> parenRelation

parenRelation ∷ ∀ m t. SqlParser m t (Sig.Relation t)
parenRelation = do
  _ ← operator "("
  r ← relation
  _ ← operator ")"
  pure r

tableRelation ∷ ∀ m t. SqlParser m t (Sig.Relation t)
tableRelation = do
  i ← ident
  path ← Pt.parseAnyPath P.fail i
  a ← PC.optionMaybe do
    _ ← keyword "as"
    ident
  pure $ Sig.TableRelation { alias: Ident <$> a, path }

variRelation ∷ ∀ m t. SqlParser m t (Sig.Relation t)
variRelation = do
  vari ← variableString
  a ← PC.optionMaybe do
    _ ← keyword "as"
    ident
  pure $ Sig.VariRelation { alias: Ident <$> a, vari }

exprRelation ∷ ∀ m t. SqlParser m t (Sig.Relation t)
exprRelation = do
  operator "("
  e ← expr
  operator ")"
  _ ← keyword "as"
  i ← ident
  pure $ Sig.ExprRelation { alias: Ident i, expr: e }

stdJoinRelation ∷ ∀ m t. SqlParser m t (Sig.Relation t → Sig.Relation t)
stdJoinRelation = do
  joinType ← joinTypes
  _ ← keyword "join"
  right ← simpleRelation
  _ ← keyword "on"
  clause ← definedExpr
  pure \left →
    Sig.JoinRelation
      { left
      , right
      , joinType
      , clause
      }
  where
  joinTypes = PC.choice
    [ keyword "left" *> PC.optional (keyword "outer") $> Sig.LeftJoin
    , keyword "right" *> PC.optional (keyword "outer") $> Sig.RightJoin
    , PC.try $ PC.optional (keyword "full") *> keyword "outer" $> Sig.FullJoin
    , keyword "full" $> Sig.FullJoin
    , PC.optional (keyword "inner") $> Sig.InnerJoin
    ]

crossJoinRelation ∷ ∀ m t. SqlParser m t (Sig.Relation t → Sig.Relation t)
crossJoinRelation = do
  _ ← keyword "cross"
  _ ← keyword "join"
  right ← simpleRelation
  pure \left →
    Sig.JoinRelation
      { joinType: Sig.InnerJoin
      , left
      , right
      , clause: C.bool true
      }

filter ∷ ∀ m t. SqlParser' m t
filter = do
  _ ← keyword "where"
  definedExpr

groupBy ∷ ∀ m t. SqlParser m t (Sig.GroupBy t)
groupBy = do
  _ ← keyword "group"
  _ ← keyword "by"
  keys ← PC.sepBy1 definedExpr $ operator ","
  having ← PC.optionMaybe do
    _ ← keyword "having"
    definedExpr
  pure $ Sig.GroupBy { keys, having }

orderBy
  ∷ ∀ t m
  . Corecursive t (Sig.SqlF EJ.EJsonF)
  ⇒ Monad m
  ⇒ L.List (Sig.Projection t)
  → P.ParserT TokenStream m (Sig.OrderBy t)
orderBy prs = do
  _ ← keyword "order"
  _ ← keyword "by"
  pos ← P.position
  lst ← flip PC.sepBy1 (operator ",") $ sortClause pos
  case lst of
    L.Nil → P.fail "incorrect order by"
    x : xs → pure $ Sig.OrderBy (x :| xs)
  where
  sortPart = PC.choice
    [ keyword "asc" $> Sig.ASC
    , keyword "desc" $> Sig.DESC
    , pure Sig.ASC
    ]

  sortClause pos = do
    mbV ← PC.optionMaybe definedExpr
    sort ← sortPart
    v ← case mbV of
      Just v' → pure v'
      Nothing → case prs of
        Sig.Projection { expr: e } : L.Nil → pure e
        _ → P.failWithPosition "ORDER BY may only omit a projection when SELECT projections have exactly one element" pos
    pure $ sort × v

projections ∷ ∀ m t. SqlParser m t (L.List (Sig.Projection t))
projections = PC.sepBy projection $ operator ","

projection ∷ ∀ m t. SqlParser m t (Sig.Projection t)
projection = do
  e ← definedExpr
  a ← PC.optionMaybe (keyword "as" *> ident)
  pure $ Sig.Projection { expr: e, alias: Ident <$> a }

_SEARCH ∷ ∀ t. Corecursive t (Sig.SqlF EJ.EJsonF) ⇒ Boolean → t → t → t
_SEARCH b lhs rhs = C.invokeFunction (Ident "SEARCH") $ lhs : rhs : (C.bool b) : L.Nil

_LIKE ∷ ∀ t. Corecursive t (Sig.SqlF EJ.EJsonF) ⇒ Maybe t → t → t → t
_LIKE mbEsc lhs rhs = C.invokeFunction (Ident "LIKE") $ lhs : rhs : (fromMaybe (C.string "\\") mbEsc) : L.Nil

_NOT ∷ ∀ t. Corecursive t (Sig.SqlF EJ.EJsonF) ⇒ t → t
_NOT = C.unop Sig.Not ∘ C.parens

_BINOP ∷ ∀ t. Corecursive t (Sig.SqlF EJ.EJsonF) ⇒ t → Sig.BinaryOperator → t → t
_BINOP = flip C.binop

_BINOP' ∷ ∀ t a. Corecursive t (Sig.SqlF EJ.EJsonF) ⇒ Sig.BinaryOperator → t → a → t → t
_BINOP' op lhs _ = C.binop op lhs
