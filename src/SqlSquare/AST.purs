module SqlSquare.AST where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either, either)
import Data.Foldable as F
import Data.Maybe (Maybe(..))
import Data.List (List, fromFoldable)
import Data.NonEmpty as NE
import Data.Tuple (Tuple(..))
import Data.Path.Pathy (AbsFile, RelFile, Unsandboxed, unsafePrintPath)

import Matryoshka (class Recursive, class Corecursive, Algebra, cata, embed, Mu)

infixr 4 type Tuple as ×
infixr 1 Tuple as ×

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

newtype GroupBy a = GroupBy { keys ∷ List a, having ∷ Maybe a }

printGroupBy ∷ Algebra GroupBy String
printGroupBy (GroupBy { keys, having }) =
  F.intercalate ", " keys <> F.foldMap (" having " <> _) having

derive instance functorGroupBy ∷ Functor GroupBy

newtype Case a = Case { cond ∷ a, expr ∷ a }

printCase ∷ Algebra Case String
printCase (Case { cond, expr }) = " when " <> cond <> " then " <> expr

derive instance functorCase ∷ Functor Case

newtype OrderBy a = OrderBy (NE.NonEmpty List (OrderType × a))

printOrderBy ∷ Algebra OrderBy String
printOrderBy (OrderBy lst) =
  F.intercalate ", " $ lst <#> \(ot × a) → printOrderType ot <> a

derive instance functorOrderBy ∷ Functor OrderBy

newtype Projection a = Projection { expr ∷ a, alias ∷ Maybe String }

printProjection ∷ Algebra Projection String
printProjection (Projection { expr, alias }) = expr <> F.foldMap (" as " <> _) alias

derive instance functorProjection ∷ Functor Projection

data SqlRelation a
  = JoinRelation
      { left ∷ SqlRelation a
      , right ∷ SqlRelation a
      , joinType ∷ JoinType
      , clause ∷ a
      }
  | ExprRelationAST
      { expr ∷ a
      , aliasName ∷ String
      }
  | TableRelationAST
      { tablePath ∷ FUPath
      , alias ∷ Maybe String
      }
  | VariRelation
      { vari ∷ String
      , alias ∷ Maybe String
      }
  | IdentRelation
      { ident ∷ String
      , alias ∷ Maybe String
      }

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

data SqlF a
  = SetLiteral (List a)
  | ArrayLiteral (List a)
  | MapLiteral (List (a × a))
  | Splice (Maybe a)
  | Binop
      { lhs ∷ a
      , rhs ∷ a
      , op ∷ BinaryOperator
      }
  | Unop
      { expr ∷ a
      , op ∷ UnaryOperator
      }
  | Ident String
  | InvokeFunction
      { name ∷ String
      , args ∷ List a
      }
  | Match
      { expr ∷ a
      , cases ∷ List (Case a)
      , default_ ∷ Maybe a
      }
  | Switch
      { cases ∷ List (Case a)
      , default_ ∷ Maybe a
      }
  | Let
      { ident ∷ String
      , bindTo ∷ a
      , in_ ∷ a
      }
  | IntLiteral Int
  | FloatLiteral Number
  | StringLiteral String
  | NullLiteral
  | BoolLiteral Boolean
  | Vari String
  | Select
      { isDistinct ∷ Boolean
      , projections ∷ List (Projection a)
      , relations ∷ Maybe (SqlRelation a)
      , filter ∷ Maybe a
      , groupBy ∷ Maybe (GroupBy a)
      , orderBy ∷ Maybe (OrderBy a)
      }

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
    Match { expr, cases, default_ } →
      Match { expr: f expr
            , cases: map (map f) cases
            , default_: map f default_
            }
    Switch { cases, default_ } →
      Switch { cases: map (map f) cases
             , default_: map f default_
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
  Match { expr, cases, default_ } →
    "case "
    <> expr
    <> F.intercalate " " (map printCase cases)
    <> F.foldMap (" else " <> _) default_
  Switch { cases, default_ } →
    "case "
    <> F.intercalate " " (map printCase cases)
    <> F.foldMap (" else " <> _) default_
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
    "select"
    <> (if isDistinct then " distinct " else "")
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
vari ∷ ∀ t. Corecursive t SqlF ⇒ String → t
vari s = embed $ Vari s

bool ∷ ∀ t. Corecursive t SqlF ⇒ Boolean → t
bool b = embed $ BoolLiteral b

null ∷ ∀ t. Corecursive t SqlF ⇒ t
null = embed NullLiteral

int ∷ ∀ t. Corecursive t SqlF ⇒ Int → t
int i = embed $ IntLiteral i

num ∷ ∀ t. Corecursive t SqlF ⇒ Number → t
num i = embed $ FloatLiteral i

unop ∷ ∀ t. Corecursive t SqlF ⇒ UnaryOperator → t → t
unop op expr = embed $ Unop { op, expr }

binop ∷ ∀ t. Corecursive t SqlF ⇒ BinaryOperator → t → t → t
binop op lhs rhs = embed $ Binop { op, lhs, rhs }

set ∷ ∀ t. (Corecursive t SqlF, F.Foldable f) ⇒ f t → t
set l = embed $ SetLiteral $ fromFoldable f

array ∷ ∀ t. (Corecursive t SqlF, F.Foldable f) ⇒ f t → t
array l = embed $ ArrayLiteral $ fromFoldable l

splice ∷ ∀ t. Corecursive t SqlF ⇒ Maybe t → t
splice m = embed $ Splice m

ident ∷ ∀ t. Corecursive t SqlF ⇒ String → t
ident i = embed $ Ident i

match ∷ ∀ t. Corecursive t SqlF ⇒ t → List (Case t) → Maybe t → t
match expr cases default_ = embed $ Match { expr, cases, default_ }

switch ∷ ∀ t. Corecursive t SqlF ⇒ List (Case t) → Maybe t → t
switch cases default_ = embed $ Switch { cases, default_ }

let_ ∷ ∀ t. Corecursive t SqlF ⇒ String → t → t → t
let_ ident bindTo in_ = embed $ Let { ident, bindTo, in_ }

-- when_ (bool true) # then_ (num 1.0) :P
when_ ∷ ∀ t. t → (t → Case t)
when_ cond = Case <<< { cond, expr: _ }

then_ ∷ ∀ t. (t → Case t) → t → Case t
then_ f t = f t

select
  ∷ ∀ t
  . Corecursive t SqlF
  ⇒ Boolean
  → List (Projection t)
  → Maybe (SqlRelation t)
  → Maybe a
  → Maybe (GroupBy a)
  → Maybe (OrderBy a)
select isDistinct projections relations filter groupBy orderBy =
  embed $ Select { isDistinct, projections, relations, filter, groupBy, orderBy }


-- project_ (ident "foo") # as_ "bar"
-- project_ (ident "foo")
project_ ∷ ∀ t. t → Projection t
project_ expr = Projection {expr, alias: Nothing}

as_ ∷ ∀ t. String → Projection t → Projection t
as_ s (Projection r) = Projection r { alias = Just s }

groupBy_ ∷ ∀ t f. Foldable f ⇒ f t → GroupBy t
groupBy_ f = GroupBy { keys: fromFoldable f, having: Nothing }

having_ ∷ ∀ t. t → GroupBy t → GroupBy t
having_ t (GroupBy r) = GroupBy r{ having = Just t }
