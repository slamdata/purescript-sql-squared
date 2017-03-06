module SqlSquare.Constructors where

import Prelude

import Data.Foldable as F
import Data.List as L
import Data.Maybe (Maybe(..))

import Matryoshka (class Corecursive, embed)

import SqlSquare.AST  (SqlF(..), Relation, GroupBy(..), OrderBy, BinaryOperator, UnaryOperator, (∘), SelectR, Case(..), Projection(..))

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

string ∷ ∀ t. Corecursive t SqlF ⇒ String → t
string s = embed $ StringLiteral s

unop ∷ ∀ t. Corecursive t SqlF ⇒ UnaryOperator → t → t
unop op expr = embed $ Unop { op, expr }

binop ∷ ∀ t. Corecursive t SqlF ⇒ BinaryOperator → t → t → t
binop op lhs rhs = embed $ Binop { op, lhs, rhs }

set ∷ ∀ t f. (Corecursive t SqlF, F.Foldable f) ⇒ f t → t
set l = embed $ SetLiteral $ L.fromFoldable l

array ∷ ∀ t f. (Corecursive t SqlF, F.Foldable f) ⇒ f t → t
array l = embed $ ArrayLiteral $ L.fromFoldable l

splice ∷ ∀ t. Corecursive t SqlF ⇒ Maybe t → t
splice m = embed $ Splice m

ident ∷ ∀ t. Corecursive t SqlF ⇒ String → t
ident i = embed $ Ident i

match ∷ ∀ t. Corecursive t SqlF ⇒ t → L.List (Case t) → Maybe t → t
match expr cases else_ = embed $ Match { expr, cases, else_ }

switch ∷ ∀ t. Corecursive t SqlF ⇒ L.List (Case t) → Maybe t → t
switch cases else_ = embed $ Switch { cases, else_ }

let_ ∷ ∀ t. Corecursive t SqlF ⇒ String → t → t → t
let_ id bindTo in_ = embed $ Let { ident: id, bindTo, in_ }

invokeFunction ∷ ∀ t. Corecursive t SqlF ⇒ String → L.List t → t
invokeFunction name args = embed $ InvokeFunction {name, args}

-- when (bool true) # then (num 1.0) :P
when ∷ ∀ t. t → (t → Case t)
when cond = Case ∘ { cond, expr: _ }

then_ ∷ ∀ t. (t → Case t) → t → Case t
then_ f t = f t

select
  ∷ ∀ t f
  . (Corecursive t SqlF, F.Foldable f)
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

buildSelect ∷ ∀ t. Corecursive t SqlF ⇒ (SelectR t → SelectR t) → t
buildSelect f =
  embed $ Select $ f { isDistinct: false
                     , projections: L.Nil
                     , relations: Nothing
                     , filter: Nothing
                     , groupBy: Nothing
                     , orderBy: Nothing
                     }

pars ∷ ∀ t. Corecursive t SqlF ⇒ t → t
pars = embed ∘ Parens
