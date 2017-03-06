module SqlSquare.Lenses where

import Data.Newtype (class Newtype, wrap, unwrap)

import Data.Lens (Prism', prism', Lens', lens, Iso', iso)

_Newtype ∷ ∀ n t. Newtype n t ⇒ Iso' n t
_Newtype = iso unwrap wrap


{-
_GroupBy ∷ ∀ a. Iso' (GroupBy a) {keys ∷ List a, having ∷ Maybe a}
_GroupBy = _Newtype

_Case ∷ ∀ a. Iso' (Case a) { cond ∷ a, expr ∷ a }
_Case = _Newtype

_OrderBy ∷ ∀ a. Iso' (OrderBy a) (NE.NonEmpty List (OrderType × a))
_OrderBy = _Newtype


_Projection ∷ ∀ a. Iso' (Projection a) { expr ∷ a, alias ∷ Maybe String }
_Projection = _Newtype


_JoinRelation ∷ ∀ a. Prism' (SqlRelation a) (JoinRelR a)
_JoinRelation = prism' JoinRelation case _ of
  JoinRelation r → Just r
  _ → Nothing

_ExprRelation ∷ ∀ a. Prism' (SqlRelation a) (ExprRelR a)
_ExprRelation = prism' ExprRelation case _ of
  ExprRelation r → Just r
  _ → Nothing

_TableRelation ∷ ∀ a. Prism' (SqlRelation a) (TableRelR a)
_TableRelation = prism' TableRelation case _ of
  TableRelation r → Just r
  _ → Nothing

_VariRelation ∷ ∀ a. Prism' (SqlRelation a) (VariRelR a)
_VariRelation = prism' VariRelation case _ of
  VariRelation r → Just r
  _ → Nothing

_IdentRelation ∷ ∀ a. Prism' (SqlRelation a) IdentRelR
_IdentRelation = prism' IdentRelation case _ of
  IdentRelation r → Just r
  _ → Nothing
-}

{-
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


-}
{-
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

_Parens ∷ ∀ t. (Recursive t SqlF, Corecursive t SqlF) ⇒ Prism' t t
_Parens = prism' (embed ∘ Parens) $ project ⋙ case _ of
  Parens t → Just t
  _ → Nothing
-}
