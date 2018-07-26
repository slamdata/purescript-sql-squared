module SqlSquared.Constructors where

import Prelude

import Data.Array as Arr
import Data.Foldable as F
import Data.HugeInt as HI
import Data.HugeNum as HN
import Data.Json.Extended.Signature (EJsonF(..), EJsonMap(..))
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Matryoshka (class Corecursive, embed)
import SqlSquared.Signature as Sig
import SqlSquared.Utils ((∘))

vari ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ Sig.Ident → t
vari = embed ∘ Sig.Vari

bool ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ Boolean → t
bool = embed ∘ Sig.Literal ∘ Boolean

null ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ t
null = embed $ Sig.Literal Null

int ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ Int → t
int = embed ∘ Sig.Literal ∘ Integer ∘ HI.fromInt

num ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ Number → t
num = embed ∘ Sig.Literal ∘ Decimal ∘ HN.fromNumber

hugeNum ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ HN.HugeNum → t
hugeNum = embed ∘ Sig.Literal ∘ Decimal

string ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ String → t
string = embed ∘ Sig.Literal ∘ String

unop ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ Sig.UnaryOperator → t → t
unop op expr = embed $ Sig.Unop { op, expr }

binop ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ Sig.BinaryOperator → t → t → t
binop op lhs rhs = embed $ Sig.Binop { op, lhs, rhs }

set ∷ ∀ t f g. Corecursive t (Sig.SqlF g) ⇒ F.Foldable f ⇒ f t → t
set = embed ∘ Sig.SetLiteral ∘ L.fromFoldable

array ∷ ∀ t f. Corecursive t (Sig.SqlF EJsonF) ⇒ F.Foldable f ⇒ f t → t
array = embed ∘ Sig.Literal ∘ Array ∘ Arr.fromFoldable

map_ ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ Ord t ⇒ Map.Map t t → t
map_ = embed ∘ Sig.Literal ∘ Map ∘ EJsonMap ∘ Map.toUnfoldable

splice ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ Maybe t → t
splice = embed ∘ Sig.Splice

ident ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ String → t
ident = ident' ∘ Sig.Ident

ident' ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ Sig.Ident → t
ident' = embed ∘ Sig.Identifier

match ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ t → L.List (Sig.Case t) → Maybe t → t
match expr cases else_ = embed $ Sig.Match { expr, cases, else_ }

switch ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ L.List (Sig.Case t) → Maybe t → t
switch cases else_ = embed $ Sig.Switch { cases, else_ }

let_ ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ Sig.Ident → t → t → t
let_ id bindTo in_ = embed $ Sig.Let { ident: id, bindTo, in_ }

invokeFunction ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ Sig.Ident → L.List t → t
invokeFunction name args = embed $ Sig.InvokeFunction {name, args}

-- when (bool true) # then_ (num 1.0) :P
when ∷ ∀ t. t → (t → Sig.Case t)
when cond = Sig.Case ∘ { cond, expr: _ }

then_ ∷ ∀ t. t → (t → Sig.Case t) → Sig.Case t
then_ t f = f t

select
  ∷ ∀ t f
  . Corecursive t (Sig.SqlF EJsonF)
  ⇒ F.Foldable f
  ⇒ Boolean
  → f (Sig.Projection t)
  → Maybe (Sig.Relation t)
  → Maybe t
  → Maybe (Sig.GroupBy t)
  → Maybe (Sig.OrderBy t)
  → t
select isDistinct projections relations filter gb orderBy =
  embed
  $ Sig.Select
    { isDistinct
    , projections: L.fromFoldable projections
    , relations
    , filter
    , groupBy: gb
    , orderBy
    }


-- project (ident "foo") # as "bar"
-- project (ident "foo")
projection ∷ ∀ t. t → Sig.Projection t
projection expr = Sig.Projection {expr, alias: Nothing}

as ∷ ∀ t. String → Sig.Projection t → Sig.Projection t
as = as' ∘ Sig.Ident

as' ∷ ∀ t. Sig.Ident → Sig.Projection t → Sig.Projection t
as' s (Sig.Projection r) = Sig.Projection r { alias = Just s }

groupBy ∷ ∀ t f. F.Foldable f ⇒ f t → Sig.GroupBy t
groupBy f = Sig.GroupBy { keys: L.fromFoldable f, having: Nothing }

having ∷ ∀ t. t → Sig.GroupBy t → Sig.GroupBy t
having t (Sig.GroupBy r) = Sig.GroupBy r{ having = Just t }

buildSelect ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ (Sig.SelectR t → Sig.SelectR t) → t
buildSelect f =
  embed
  $ Sig.Select
  $ f { isDistinct: false
      , projections: L.Nil
      , relations: Nothing
      , filter: Nothing
      , groupBy: Nothing
      , orderBy: Nothing
      }

parens ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ t → t
parens = embed ∘ Sig.Parens
