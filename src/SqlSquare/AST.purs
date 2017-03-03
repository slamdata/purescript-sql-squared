module SqlSquare.AST where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either, either)
import Data.Foldable as F
import Data.Functor.Mu (Mu)
import Data.Maybe (Maybe(..))
import Data.List (List(..), fromFoldable)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.NonEmpty as NE
import Data.Tuple (Tuple(..))
import Data.Path.Pathy (AbsFile, RelFile, Unsandboxed, unsafePrintPath)
import Data.Lens (Prism', prism', Lens', lens, Iso', iso)

import Matryoshka (class Recursive, class Corecursive, Algebra, cata, embed, project)

infixr 4 type Tuple as ×
infixr 1 Tuple as ×
infixr 9 compose as ∘

composeFlipped ∷ ∀ a b c d. Semigroupoid a ⇒ a b c → a c d → a b d
composeFlipped f g = compose g f

infixr 9 composeFlipped as ⋙

type FUPath = Either (RelFile Unsandboxed) (AbsFile Unsandboxed)

data OrderType
  = ASC
  | DESC

printOrderType ∷ OrderType → String
printOrderType = case _ of
  ASC → "asc"
  DESC → "desc"

derive instance eqOrderType ∷ Eq OrderType

data JoinType
  = LeftJoin
  | RightJoin
  | InnerJoin
  | FullJoin

printJoinType ∷ JoinType → String
printJoinType = case _ of
  LeftJoin → "left join"
  RightJoin → "right join"
  FullJoin → "full join"
  InnerJoin → "inner join"

derive instance eqJoinType ∷ Eq JoinType

data BinaryOperator
  = IfUndefined
  | Range
  | Or
  | And
  | Eq
  | Neq
  | Ge
  | Gt
  | Le
  | Lt
  | Concat
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Pow
  | In
  | FieldDeref
  | IndexDeref
  | Limit
  | Offset
  | Sample
  | Union
  | UnionAll
  | Intersect
  | IntersectAll
  | Except
  | UnshiftMap

derive instance eqBinaryOperator ∷ Eq BinaryOperator

data UnaryOperator
  = Not
  | Exists
  | Positive
  | Negative
  | Distinct
  | FlattenMapKeys
  | FlattenMapValues
  | ShiftMapKeys
  | ShiftMapValues
  | FlattenArrayIndices
  | FlattenArrayValues
  | ShiftArrayIndices
  | ShiftArrayValues
  | UnshiftArray

derive instance eqUnaryOperator ∷ Eq UnaryOperator

_Newtype ∷ ∀ n t. Newtype n t ⇒ Iso' n t
_Newtype = iso unwrap wrap

newtype GroupBy a = GroupBy { keys ∷ List a, having ∷ Maybe a }
derive instance newtypeGroupBy ∷ Newtype (GroupBy a) _
derive instance functorGroupBy ∷ Functor GroupBy

_GroupBy ∷ ∀ a. Iso' (GroupBy a) {keys ∷ List a, having ∷ Maybe a}
_GroupBy = _Newtype

printGroupBy ∷ Algebra GroupBy String
printGroupBy (GroupBy { keys, having }) =
  F.intercalate ", " keys <> F.foldMap (" having " <> _) having


newtype Case a = Case { cond ∷ a, expr ∷ a }

derive instance functorCase ∷ Functor Case
derive instance newtypeCase ∷ Newtype (Case a) _

_Case ∷ ∀ a. Iso' (Case a) { cond ∷ a, expr ∷ a }
_Case = _Newtype

printCase ∷ Algebra Case String
printCase (Case { cond, expr }) = " when " <> cond <> " then " <> expr


newtype OrderBy a = OrderBy (NE.NonEmpty List (OrderType × a))

derive instance functorOrderBy ∷ Functor OrderBy
derive instance newtypeOrderBy ∷ Newtype (OrderBy a) _

_OrderBy ∷ ∀ a. Iso' (OrderBy a) (NE.NonEmpty List (OrderType × a))
_OrderBy = _Newtype

printOrderBy ∷ Algebra OrderBy String
printOrderBy (OrderBy lst) =
  F.intercalate ", " $ lst <#> \(ot × a) → printOrderType ot <> " " <> a

newtype Projection a = Projection { expr ∷ a, alias ∷ Maybe String }

derive instance functorProjection ∷ Functor Projection
derive instance newtypeProjection ∷ Newtype (Projection a) _

_Projection ∷ ∀ a. Iso' (Projection a) { expr ∷ a, alias ∷ Maybe String }
_Projection = _Newtype

printProjection ∷ Algebra Projection String
printProjection (Projection { expr, alias }) = expr <> F.foldMap (" as " <> _) alias


type JoinRelR a =
  { left ∷ SqlRelation a
  , right ∷ SqlRelation a
  , joinType ∷ JoinType
  , clause ∷ a
  }

type ExprRelR a =
  { expr ∷ a
  , aliasName ∷ String
  }

type TableRelR a =
  { tablePath ∷ FUPath
  , alias ∷ Maybe String
  }

type VariRelR a =
  { vari ∷ String
  , alias ∷ Maybe String
  }

type IdentRelR =
  { ident ∷ String
  , alias ∷ Maybe String
  }

data SqlRelation a
  = JoinRelation (JoinRelR a)
  | ExprRelationAST (ExprRelR a)
  | TableRelationAST (TableRelR a)
  | VariRelation (VariRelR a)
  | IdentRelation IdentRelR

_JoinRelation ∷ ∀ a. Prism' (SqlRelation a) (JoinRelR a)
_JoinRelation = prism' JoinRelation case _ of
  JoinRelation r → Just r
  _ → Nothing

_ExprRelation ∷ ∀ a. Prism' (SqlRelation a) (ExprRelR a)
_ExprRelation = prism' ExprRelationAST case _ of
  ExprRelationAST r → Just r
  _ → Nothing

_TableRelation ∷ ∀ a. Prism' (SqlRelation a) (TableRelR a)
_TableRelation = prism' TableRelationAST case _ of
  TableRelationAST r → Just r
  _ → Nothing

_VariRelation ∷ ∀ a. Prism' (SqlRelation a) (VariRelR a)
_VariRelation = prism' VariRelation case _ of
  VariRelation r → Just r
  _ → Nothing

_IdentRelation ∷ ∀ a. Prism' (SqlRelation a) IdentRelR
_IdentRelation = prism' IdentRelation case _ of
  IdentRelation r → Just r
  _ → Nothing

derive instance functorSqlRelation ∷ Functor SqlRelation

printRelation ∷ Algebra SqlRelation String
printRelation = case _ of
  ExprRelationAST {expr, aliasName} →
    "(" <> expr <> ") as " <> aliasName
  VariRelation { vari, alias} →
    vari <> F.foldMap (" as " <> _) alias
  TableRelationAST { tablePath, alias } →
    "`"
    <> either unsafePrintPath unsafePrintPath tablePath
    <> "`"
    <> F.foldMap (" as " <> _) alias
  IdentRelation { ident, alias } →
    ident <> F.foldMap (" as " <> _) alias
  JoinRelation { left, right, joinType, clause } →
    printRelation left
    <> " "
    <> printJoinType joinType
    <> " "
    <> printRelation right
    <> " on "
    <> clause

type BinopR a =
  { lhs ∷ a
  , rhs ∷ a
  , op ∷ BinaryOperator
  }

type UnopR a =
  { expr ∷ a
  , op ∷ UnaryOperator
  }

type InvokeFunctionR a =
  { name ∷ String
  , args ∷ List a
  }

type MatchR a =
  { expr ∷ a
  , cases ∷ List (Case a)
  , else_ ∷ Maybe a
  }

type SwitchR a =
  { cases ∷ List (Case a)
  , else_ ∷ Maybe a
  }

type LetR a =
  { ident ∷ String
  , bindTo ∷ a
  , in_ ∷ a
  }

type SelectR a =
  { isDistinct ∷  Boolean
  , projections ∷ List (Projection a)
  , relations ∷ Maybe (SqlRelation a)
  , filter ∷ Maybe a
  , groupBy ∷ Maybe (GroupBy a)
  , orderBy ∷ Maybe (OrderBy a)
  }


_lhs ∷ ∀ a r. Lens' { lhs ∷ a |r } a
_lhs = lens _.lhs _{ lhs = _ }

_rhs ∷ ∀ a r. Lens' { rhs ∷ a |r } a
_rhs = lens _.rhs _{ rhs = _ }

_op ∷ ∀ a r. Lens' { op ∷ a | r } a
_op = lens _.op _{ op = _ }

_expr ∷ ∀ a r. Lens' { expr ∷ a|r } a
_expr = lens _.expr _{ expr = _ }

_name ∷ ∀ a r. Lens' { name ∷ a|r } a
_name = lens _.name _{ name = _ }

_args ∷ ∀ a r. Lens' { args ∷ a|r } a
_args = lens _.args _{ args = _ }

_cases ∷ ∀ a r. Lens' { cases ∷ a|r } a
_cases = lens _.cases _{ cases = _ }

_else ∷ ∀ a r. Lens' { else_ ∷ a|r } a
_else = lens _.else_ _{ else_ = _ }

_ident ∷ ∀ a r. Lens' { ident ∷ a|r } a
_ident = lens _.ident _{ ident = _ }

_bindTo ∷ ∀ a r. Lens' { bindTo ∷ a|r } a
_bindTo = lens _.bindTo _{ bindTo = _ }

_in ∷ ∀ a r. Lens' { in_ ∷ a|r } a
_in = lens _.in_ _{ in_ = _ } -- __O_M_G__

_isDistinct ∷ ∀ a r. Lens' { isDistinct ∷ a|r } a
_isDistinct = lens _.isDistinct _{ isDistinct = _ }

_projections ∷ ∀ a r. Lens' { projections ∷ a|r } a
_projections = lens _.projections _{ projections = _ }

_relations ∷ ∀ a r. Lens' { relations ∷ a|r } a
_relations = lens _.relations _{ relations = _ }

_filter ∷ ∀ a r. Lens' { filter ∷ a|r } a
_filter = lens _.filter _{ filter = _ }

_groupBy ∷ ∀ a r. Lens' { groupBy ∷ a|r } a
_groupBy = lens _.groupBy _{ groupBy = _ }

_orderBy ∷ ∀ a r. Lens' { orderBy ∷ a|r } a
_orderBy = lens _.orderBy _{ orderBy = _ }

_keys ∷ ∀ a r. Lens' { keys ∷ a|r } a
_keys = lens _.keys _{ keys = _ }

_having ∷ ∀ a r. Lens' { having ∷ a|r } a
_having = lens _.having _{ having = _ }

_cond ∷ ∀ a r. Lens' { cond ∷ a|r } a
_cond = lens _.cond _{ cond = _ }

_alias ∷ ∀ a r. Lens' { alias ∷ a|r } a
_alias = lens _.alias _{ alias = _ }

_aliasName ∷ ∀ a r. Lens' { aliasName ∷ a|r } a
_aliasName = lens _.aliasName _{ aliasName = _ }

_left ∷ ∀ a r. Lens' { left ∷ a|r } a
_left = lens _.left _{ left = _ }

_right ∷ ∀ a r. Lens' { right ∷ a|r } a
_right = lens _.right _{ right = _ }

_joinType ∷ ∀ a r. Lens' { joinType ∷ a|r } a
_joinType = lens _.joinType _{ joinType = _ }

_clause ∷ ∀ a r. Lens' { clause ∷ a|r } a
_clause = lens _.clause _{ clause = _ }

_tablePath ∷ ∀ a r. Lens' { tablePath ∷ a|r } a
_tablePath = lens _.tablePath _{ tablePath = _ }

data SqlF a
  = SetLiteral (List a)
  | ArrayLiteral (List a)
  | MapLiteral (List (a × a))
  | Splice (Maybe a)
  | Binop (BinopR a)
  | Unop (UnopR a)
  | Ident String
  | InvokeFunction (InvokeFunctionR a)
  | Match (MatchR a)
  | Switch (SwitchR a)
  | Let (LetR a)
  | IntLiteral Int
  | FloatLiteral Number
  | StringLiteral String
  | NullLiteral
  | BoolLiteral Boolean
  | Vari String
  | Select (SelectR a)


_SetLiteral ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (List t)
_SetLiteral = prism' (embed ∘ SetLiteral) $ project ⋙ case _ of
  SetLiteral lst → Just lst
  _ → Nothing

_ArrayLiteral ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (List t)
_ArrayLiteral = prism' (embed ∘ ArrayLiteral) $ project ⋙ case _ of
  ArrayLiteral lst → Just lst
  _ → Nothing

_MapLiteral ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (List (t × t))
_MapLiteral = prism' (embed ∘ MapLiteral) $ project ⋙ case _ of
  MapLiteral tpls → Just tpls
  _ → Nothing

_Splice ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (Maybe t)
_Splice = prism' (embed ∘ Splice) $ project ⋙ case _ of
  Splice m → Just m
  _ → Nothing

_Binop ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (BinopR t)
_Binop = prism' (embed ∘ Binop) $ project ⋙ case _ of
  Binop b → Just b
  _ → Nothing

_Unop ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (UnopR t)
_Unop = prism' (embed ∘ Unop) $ project ⋙ case _ of
  Unop r → Just r
  _ → Nothing

_Ident ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t String
_Ident = prism' (embed ∘ Ident) $ project ⋙ case _ of
  Ident s → Just s
  _ → Nothing

_InvokeFunction ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (InvokeFunctionR t)
_InvokeFunction = prism' (embed ∘ InvokeFunction) $ project ⋙ case _ of
  InvokeFunction r → Just r
  _ → Nothing

_Match ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (MatchR t)
_Match = prism' (embed ∘ Match) $ project ⋙ case _ of
  Match r → Just r
  _ → Nothing

_Switch ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (SwitchR t)
_Switch = prism' (embed ∘ Switch) $ project ⋙ case _ of
  Switch r → Just r
  _ → Nothing

_Let ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (LetR t)
_Let = prism' (embed ∘ Let) $ project ⋙ case _ of
  Let r → Just r
  _ → Nothing

_IntLiteral ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t Int
_IntLiteral = prism' (embed ∘ IntLiteral) $ project ⋙ case _ of
  IntLiteral r → Just r
  _ → Nothing

_FloatLiteral ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t Number
_FloatLiteral = prism' (embed ∘ FloatLiteral) $ project ⋙ case _ of
  FloatLiteral r → Just r
  _ → Nothing

_StringLiteral ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t String
_StringLiteral = prism' (embed ∘ StringLiteral) $ project ⋙ case _ of
  StringLiteral r → Just r
  _ → Nothing

_NullLiteral ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t Unit
_NullLiteral = prism' (const $ embed $ NullLiteral) $ project ⋙ case _ of
  NullLiteral → Just unit
  _ → Nothing

_BoolLiteral ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t Boolean
_BoolLiteral = prism' (embed ∘ BoolLiteral) $ project ⋙ case _ of
  BoolLiteral b → Just b
  _ → Nothing

_Vari ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t String
_Vari = prism' (embed ∘ Vari) $ project ⋙ case _ of
  Vari r → Just r
  _ → Nothing

_Select ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t (SelectR t)
_Select = prism' (embed ∘ Select) $ project ⋙ case _ of
  Select r → Just r
  _ → Nothing


instance functorAST ∷ Functor SqlF where
  map f = case _ of
    Select { isDistinct, projections, relations, filter, groupBy, orderBy } →
      Select { isDistinct
             , projections: map (map f) projections
             , relations: map (map f) relations
             , filter: map f filter
             , groupBy: map (map f) groupBy
             , orderBy: map (map f) orderBy
             }
    Vari s →
      Vari s
    BoolLiteral b →
      BoolLiteral b
    NullLiteral →
      NullLiteral
    StringLiteral s →
      StringLiteral s
    FloatLiteral n →
      FloatLiteral n
    IntLiteral i →
      IntLiteral i
    Let { ident, bindTo, in_ } →
      Let { ident
          , bindTo: f bindTo
          , in_: f in_
          }
    MapLiteral lst →
      MapLiteral $ map (bimap f f) lst
    Splice a →
      Splice $ map f a
    Binop { lhs, rhs, op } →
      Binop { lhs: f lhs
            , rhs: f rhs
            , op
            }
    Unop { expr, op } →
      Unop { expr: f expr
           , op
           }
    Ident s →
      Ident s
    InvokeFunction { name, args } →
      InvokeFunction { name
                     , args: map f args
                     }
    Match { expr, cases, else_ } →
      Match { expr: f expr
            , cases: map (map f) cases
            , else_: map f else_
            }
    Switch { cases, else_ } →
      Switch { cases: map (map f) cases
             , else_: map f else_
             }
    SetLiteral lst →
      SetLiteral $ map f lst
    ArrayLiteral lst →
      ArrayLiteral $ map f lst


printF ∷ Algebra SqlF String
printF = case _ of
  SetLiteral lst → "(" <> F.intercalate ", " lst <> ")"
  ArrayLiteral lst → "[" <> F.intercalate ", " lst <> "]"
  MapLiteral tplLst → "{" <> F.intercalate ", " (map (\(k × v) → k <> ": " <> v) tplLst) <> "}"
  Splice mb → case mb of
    Nothing → "*"
    Just a → a <> ".*"
  Binop {lhs, rhs, op} → case op of
    IfUndefined → lhs <> " ?? " <> rhs
    Range → lhs <> " .. " <> rhs
    Or → lhs <> " or " <> rhs
    And → lhs <> " and " <> rhs
    Eq → lhs <> " = " <> rhs
    Neq → lhs <> " <> " <> rhs
    Ge → lhs <> " >= " <> rhs
    Gt → lhs <> " > " <> rhs
    Le → lhs <> " <= " <> rhs
    Lt → lhs <> " < " <> rhs
    Concat → lhs <> " || " <> rhs
    Plus → lhs <> " + " <> rhs
    Minus → lhs <> " - " <> rhs
    Mult → lhs <> " * " <> rhs
    Div → lhs <> " / " <> rhs
    Mod → lhs <> " % " <> rhs
    Pow → lhs <> " ^ " <> rhs
    In → lhs <> " in " <> rhs
    FieldDeref → lhs <> "." <> rhs
    IndexDeref → lhs <> "[" <> rhs <> "]"
    Limit → lhs <> " limit " <> rhs
    Offset → lhs <> " offset " <> rhs
    Sample → lhs <> " sample " <> rhs
    Union → lhs <> " union " <> rhs
    UnionAll → lhs <> " union all " <> rhs
    Intersect → lhs <> " intersect " <> rhs
    IntersectAll → lhs <> " intersect all " <> rhs
    Except → lhs <> " except " <> rhs
    UnshiftMap → "{" <> lhs <> ": " <> rhs <> "...}"
  Unop {expr, op} → case op of
    Not → "not " <> expr
    Exists → "exists " <> expr
    Positive → "+" <> expr
    Negative → "-" <> expr
    Distinct → "distinct " <> expr
    FlattenMapKeys → expr <> "{*: }"
    FlattenMapValues → expr <> "{*}"
    ShiftMapKeys → expr <> "{_: }"
    ShiftMapValues → expr <> "{_}"
    FlattenArrayIndices → expr <> "[*:]"
    FlattenArrayValues → expr <> "[*]"
    ShiftArrayIndices → expr <> "[_:]"
    ShiftArrayValues → expr <> "[_]"
    UnshiftArray → "[" <> expr <> "...]"
  Ident s →
    s
  InvokeFunction {name, args} →
    name <> "(" <> F.intercalate "," args <> ")"
  Match { expr, cases, else_ } →
    "case "
    <> expr
    <> F.intercalate " " (map printCase cases)
    <> F.foldMap (" else " <> _) else_
  Switch { cases, else_ } →
    "case "
    <> F.intercalate " " (map printCase cases)
    <> F.foldMap (" else " <> _) else_
  Let { ident, bindTo, in_ } →
    ident <> " := " <> bindTo <> "; " <> in_
  IntLiteral int →
    show int
  FloatLiteral n →
    show n
  StringLiteral s →
    show s
  NullLiteral →
    "null"
  BoolLiteral b →
    show b
  Vari s →
    ":" <> s
  Select { isDistinct, projections, relations, filter, groupBy, orderBy } →
    "select "
    <> (if isDistinct then "distinct " else "")
    <> (F.intercalate ", " $ map printProjection projections)
    <> (relations # F.foldMap \rs →
         " from " <> printRelation rs)
    <> (filter # F.foldMap \f → " where " <> f)
    <> (groupBy # F.foldMap \gb → " group by " <> printGroupBy gb)
    <> (orderBy # F.foldMap \ob → " order by " <> printOrderBy ob)


type Sql = Mu SqlF

print ∷ ∀ t. Recursive t SqlF ⇒ t → String
print = cata printF

-- | constructors
vari_ ∷ ∀ t. Corecursive t SqlF ⇒ String → t
vari_ s = embed $ Vari s

bool_ ∷ ∀ t. Corecursive t SqlF ⇒ Boolean → t
bool_ b = embed $ BoolLiteral b

null_ ∷ ∀ t. Corecursive t SqlF ⇒ t
null_ = embed NullLiteral

int_ ∷ ∀ t. Corecursive t SqlF ⇒ Int → t
int_ i = embed $ IntLiteral i

num_ ∷ ∀ t. Corecursive t SqlF ⇒ Number → t
num_ i = embed $ FloatLiteral i

unop_ ∷ ∀ t. Corecursive t SqlF ⇒ UnaryOperator → t → t
unop_ op expr = embed $ Unop { op, expr }

binop_ ∷ ∀ t. Corecursive t SqlF ⇒ BinaryOperator → t → t → t
binop_ op lhs rhs = embed $ Binop { op, lhs, rhs }

set_ ∷ ∀ t f. (Corecursive t SqlF, F.Foldable f) ⇒ f t → t
set_ l = embed $ SetLiteral $ fromFoldable l

array_ ∷ ∀ t f. (Corecursive t SqlF, F.Foldable f) ⇒ f t → t
array_ l = embed $ ArrayLiteral $ fromFoldable l

splice_ ∷ ∀ t. Corecursive t SqlF ⇒ Maybe t → t
splice_ m = embed $ Splice m

ident_ ∷ ∀ t. Corecursive t SqlF ⇒ String → t
ident_ i = embed $ Ident i

match_ ∷ ∀ t. Corecursive t SqlF ⇒ t → List (Case t) → Maybe t → t
match_ expr cases else_ = embed $ Match { expr, cases, else_ }

switch_ ∷ ∀ t. Corecursive t SqlF ⇒ List (Case t) → Maybe t → t
switch_ cases else_ = embed $ Switch { cases, else_ }

let_ ∷ ∀ t. Corecursive t SqlF ⇒ String → t → t → t
let_ ident bindTo in_ = embed $ Let { ident, bindTo, in_ }

invokeFunction_ ∷ ∀ t. Corecursive t SqlF ⇒ String → List t → t
invokeFunction_ name args = embed $ InvokeFunction {name, args}

-- when_ (bool true) # then_ (num 1.0) :P
when_ ∷ ∀ t. t → (t → Case t)
when_ cond = Case ∘ { cond, expr: _ }

then_ ∷ ∀ t. (t → Case t) → t → Case t
then_ f t = f t

select_
  ∷ ∀ t f
  . (Corecursive t SqlF, F.Foldable f)
  ⇒ Boolean
  → f (Projection t)
  → Maybe (SqlRelation t)
  → Maybe t
  → Maybe (GroupBy t)
  → Maybe (OrderBy t)
  → t
select_ isDistinct projections relations filter groupBy orderBy =
  embed $ Select { isDistinct
                 , projections: fromFoldable projections
                 , relations
                 , filter
                 , groupBy
                 , orderBy
                 }


select ∷ ∀ t f. Corecursive t SqlF ⇒ SelectR t → t
select = embed ∘ Select

-- project_ (ident "foo") # as_ "bar"
-- project_ (ident "foo")
project_ ∷ ∀ t. t → Projection t
project_ expr = Projection {expr, alias: Nothing}

as_ ∷ ∀ t. String → Projection t → Projection t
as_ s (Projection r) = Projection r { alias = Just s }

groupBy_ ∷ ∀ t f. F.Foldable f ⇒ f t → GroupBy t
groupBy_ f = GroupBy { keys: fromFoldable f, having: Nothing }

having_ ∷ ∀ t. t → GroupBy t → GroupBy t
having_ t (GroupBy r) = GroupBy r{ having = Just t }

buildSelect ∷ ∀ t. Corecursive t SqlF ⇒ (SelectR t → SelectR t) → t
buildSelect f =
  select $ f { isDistinct: false
             , projections: Nil
             , relations: Nothing
             , filter: Nothing
             , groupBy: Nothing
             , orderBy: Nothing
             }
