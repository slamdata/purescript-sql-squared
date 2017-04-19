module SqlSquare.Constructors where

import Prelude

import Data.Array as Arr
import Data.Json.Extended.Signature (EJsonF(..), EJsonMap(..))
import Data.Foldable as F
import Data.HugeNum as HN
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..))

import Matryoshka (class Corecursive, embed)

import SqlSquare.Signature as Sig
import SqlSquare.Utils ((∘))

vari ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ String → t
vari s = embed $ Sig.Vari s

bool ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ Boolean → t
bool b = embed $ Sig.Literal $ Boolean b

null ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ t
null = embed $ Sig.Literal Null

int ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ Int → t
int i = embed $ Sig.Literal $ Integer i

num ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ Number → t
num i = embed $ Sig.Literal $ Decimal $ HN.fromNumber i

hugeNum ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ HN.HugeNum → t
hugeNum hn = embed $ Sig.Literal $ Decimal hn

string ∷ ∀ t. Corecursive t (Sig.SqlF EJsonF) ⇒ String → t
string s = embed $ Sig.Literal $ String s

unop ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ Sig.UnaryOperator → t → t
unop op expr = embed $ Sig.Unop { op, expr }

binop ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ Sig.BinaryOperator → t → t → t
binop op lhs rhs = embed $ Sig.Binop { op, lhs, rhs }

set ∷ ∀ t f g. (Corecursive t (Sig.SqlF g), F.Foldable f) ⇒ f t → t
set l = embed $ Sig.SetLiteral $ L.fromFoldable l

array ∷ ∀ t f. (Corecursive t (Sig.SqlF EJsonF), F.Foldable f) ⇒ f t → t
array l = embed $ Sig.Literal $ Array $ Arr.fromFoldable l

map_ ∷ ∀ t. (Corecursive t (Sig.SqlF EJsonF), Ord t) ⇒ Map.Map t t → t
map_ m = embed $ Sig.Literal $ Map ∘ EJsonMap $ Arr.fromFoldable $ Map.toList m

splice ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ Maybe t → t
splice m = embed $ Sig.Splice m

ident ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ String → t
ident i = embed $ Sig.Ident i

match ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ t → L.List (Sig.Case t) → Maybe t → t
match expr cases else_ = embed $ Sig.Match { expr, cases, else_ }

switch ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ L.List (Sig.Case t) → Maybe t → t
switch cases else_ = embed $ Sig.Switch { cases, else_ }

let_ ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ String → t → t → t
let_ id bindTo in_ = embed $ Sig.Let { ident: id, bindTo, in_ }

invokeFunction ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ String → L.List t → t
invokeFunction name args = embed $ Sig.InvokeFunction {name, args}

-- when (bool true) # then_ (num 1.0) :P
when ∷ ∀ t. t → (t → Sig.Case t)
when cond = Sig.Case ∘ { cond, expr: _ }

then_ ∷ ∀ t. t → (t → Sig.Case t) → Sig.Case t
then_ t f = f t

select
  ∷ ∀ t f
  . (Corecursive t (Sig.SqlF EJsonF), F.Foldable f)
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
as s (Sig.Projection r) = Sig.Projection r { alias = Just s }

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

pars ∷ ∀ t f. Corecursive t (Sig.SqlF f) ⇒ t → t
pars = embed ∘ Sig.Parens
