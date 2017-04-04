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

import SqlSquare.AST  (SqlF(..), Relation, GroupBy(..), OrderBy, BinaryOperator, UnaryOperator, (∘), SelectR, Case(..), Projection(..))

vari ∷ ∀ t f. Corecursive t (SqlF f) ⇒ String → t
vari s = embed $ Vari s

bool ∷ ∀ t. Corecursive t (SqlF EJsonF) ⇒ Boolean → t
bool b = embed $ Literal $ Boolean b

null ∷ ∀ t. Corecursive t (SqlF EJsonF) ⇒ t
null = embed $ Literal Null

int ∷ ∀ t. Corecursive t (SqlF EJsonF) ⇒ Int → t
int i = embed $ Literal $ Integer i

num ∷ ∀ t. Corecursive t (SqlF EJsonF) ⇒ Number → t
num i = embed $ Literal $ Decimal $ HN.fromNumber i

string ∷ ∀ t. Corecursive t (SqlF EJsonF) ⇒ String → t
string s = embed $ Literal $ String s

unop ∷ ∀ t f. Corecursive t (SqlF f) ⇒ UnaryOperator → t → t
unop op expr = embed $ Unop { op, expr }

binop ∷ ∀ t f. Corecursive t (SqlF f) ⇒ BinaryOperator → t → t → t
binop op lhs rhs = embed $ Binop { op, lhs, rhs }

set ∷ ∀ t f g. (Corecursive t (SqlF g), F.Foldable f) ⇒ f t → t
set l = embed $ SetLiteral $ L.fromFoldable l

array ∷ ∀ t f. (Corecursive t (SqlF EJsonF), F.Foldable f) ⇒ f t → t
array l = embed $ Literal $ Array $ Arr.fromFoldable l

map_ ∷ ∀ t. (Corecursive t (SqlF EJsonF), Ord t) ⇒ Map.Map t t → t
map_ m = embed $ Literal $ Map ∘ EJsonMap $ Arr.fromFoldable $ Map.toList m

splice ∷ ∀ t f. Corecursive t (SqlF f) ⇒ Maybe t → t
splice m = embed $ Splice m

ident ∷ ∀ t f. Corecursive t (SqlF f) ⇒ String → t
ident i = embed $ Ident i

match ∷ ∀ t f. Corecursive t (SqlF f) ⇒ t → L.List (Case t) → Maybe t → t
match expr cases else_ = embed $ Match { expr, cases, else_ }

switch ∷ ∀ t f. Corecursive t (SqlF f) ⇒ L.List (Case t) → Maybe t → t
switch cases else_ = embed $ Switch { cases, else_ }

let_ ∷ ∀ t f. Corecursive t (SqlF f) ⇒ String → t → t → t
let_ id bindTo in_ = embed $ Let { ident: id, bindTo, in_ }

invokeFunction ∷ ∀ t f. Corecursive t (SqlF f) ⇒ String → L.List t → t
invokeFunction name args = embed $ InvokeFunction {name, args}

-- when (bool true) # then_ (num 1.0) :P
when ∷ ∀ t. t → (t → Case t)
when cond = Case ∘ { cond, expr: _ }

then_ ∷ ∀ t. (t → Case t) → t → Case t
then_ f t = f t

select
  ∷ ∀ t f
  . (Corecursive t (SqlF EJsonF), F.Foldable f)
  ⇒ Boolean
  → f (Projection t)
  → Maybe (Relation t)
  → Maybe t
  → Maybe (GroupBy t)
  → Maybe (OrderBy t)
  → t
select isDistinct projections relations filter gb orderBy =
  embed $ Select { isDistinct
                 , projections: L.fromFoldable projections
                 , relations
                 , filter
                 , groupBy: gb
                 , orderBy
                 }


-- project (ident "foo") # as "bar"
-- project (ident "foo")
projection ∷ ∀ t. t → Projection t
projection expr = Projection {expr, alias: Nothing}

as ∷ ∀ t. String → Projection t → Projection t
as s (Projection r) = Projection r { alias = Just s }

groupBy ∷ ∀ t f. F.Foldable f ⇒ f t → GroupBy t
groupBy f = GroupBy { keys: L.fromFoldable f, having: Nothing }

having ∷ ∀ t. t → GroupBy t → GroupBy t
having t (GroupBy r) = GroupBy r{ having = Just t }

buildSelect ∷ ∀ t f. Corecursive t (SqlF f) ⇒ (SelectR t → SelectR t) → t
buildSelect f =
  embed $ Select $ f { isDistinct: false
                     , projections: L.Nil
                     , relations: Nothing
                     , filter: Nothing
                     , groupBy: Nothing
                     , orderBy: Nothing
                     }

pars ∷ ∀ t f. Corecursive t (SqlF f) ⇒ t → t
pars = embed ∘ Parens
