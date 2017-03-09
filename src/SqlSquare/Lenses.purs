module SqlSquare.Lenses where

import Prelude

import Data.HugeNum as HN
import Data.Json.Extended as EJ
import Data.Lens (Prism', prism', Lens', lens, Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List as L
import Data.Maybe as M
import Data.NonEmpty as NE

import Matryoshka (class Recursive, class Corecursive, embed, project)

import SqlSquare.AST as S
import SqlSquare.Utils (type (×), (∘), (⋙))

_GroupBy ∷ ∀ a. Iso' (S.GroupBy a) {keys ∷ L.List a, having ∷ M.Maybe a}
_GroupBy = _Newtype

_Case ∷ ∀ a. Iso' (S.Case a) { cond ∷ a, expr ∷ a }
_Case = _Newtype

_OrderBy ∷ ∀ a. Iso' (S.OrderBy a) (NE.NonEmpty L.List (S.OrderType × a))
_OrderBy = _Newtype

_Projection ∷ ∀ a. Iso' (S.Projection a) { expr ∷ a, alias ∷ M.Maybe String }
_Projection = _Newtype

_JoinRelation ∷ ∀ a. Prism' (S.Relation a) (S.JoinRelR a)
_JoinRelation = prism' S.JoinRelation case _ of
  S.JoinRelation r → M.Just r
  _ → M.Nothing

_ExprRelation ∷ ∀ a. Prism' (S.Relation a) (S.ExprRelR a)
_ExprRelation = prism' S.ExprRelation case _ of
  S.ExprRelation r → M.Just r
  _ → M.Nothing

_TableRelation ∷ ∀ a. Prism' (S.Relation a) (S.TableRelR a)
_TableRelation = prism' S.TableRelation case _ of
  S.TableRelation r → M.Just r
  _ → M.Nothing

_VariRelation ∷ ∀ a. Prism' (S.Relation a) (S.VariRelR a)
_VariRelation = prism' S.VariRelation case _ of
  S.VariRelation r → M.Just r
  _ → M.Nothing

_IdentRelation ∷ ∀ a. Prism' (S.Relation a) S.IdentRelR
_IdentRelation = prism' S.IdentRelation case _ of
  S.IdentRelation r → M.Just r
  _ → M.Nothing


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


_SetLiteral
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (L.List t)
_SetLiteral = prism' (embed ∘ S.SetLiteral) $ project ⋙ case _ of
  S.SetLiteral lst → M.Just lst
  _ → M.Nothing

_Literal
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (EJ.EJsonF t)
_Literal = prism' (embed ∘ S.Literal) $ project ⋙ case _ of
  S.Literal js → M.Just js
  _ → M.Nothing

_ArrayLiteral
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (Array t)
_ArrayLiteral = prism' (embed ∘ S.Literal ∘ EJ.Array) $ project ⋙ case _ of
  S.Literal (EJ.Array a) → M.Just a
  _ → M.Nothing

_MapLiteral
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (Array (t × t))
_MapLiteral = prism' (embed ∘ S.Literal ∘ EJ.Map) $ project ⋙ case _ of
  S.Literal (EJ.Map tpls) → M.Just tpls
  _ → M.Nothing

_Splice
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (M.Maybe t)
_Splice = prism' (embed ∘ S.Splice) $ project ⋙ case _ of
  S.Splice m → M.Just m
  _ → M.Nothing

_Binop
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (S.BinopR t)
_Binop = prism' (embed ∘ S.Binop) $ project ⋙ case _ of
  S.Binop b → M.Just b
  _ → M.Nothing

_Unop
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (S.UnopR t)
_Unop = prism' (embed ∘ S.Unop) $ project ⋙ case _ of
  S.Unop r → M.Just r
  _ → M.Nothing

_Ident
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t String
_Ident = prism' (embed ∘ S.Ident) $ project ⋙ case _ of
  S.Ident s → M.Just s
  _ → M.Nothing

_InvokeFunction
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (S.InvokeFunctionR t)
_InvokeFunction = prism' (embed ∘ S.InvokeFunction) $ project ⋙ case _ of
  S.InvokeFunction r → M.Just r
  _ → M.Nothing

_Match
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (S.MatchR t)
_Match = prism' (embed ∘ S.Match) $ project ⋙ case _ of
  S.Match r → M.Just r
  _ → M.Nothing

_Switch
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (S.SwitchR t)
_Switch = prism' (embed ∘ S.Switch) $ project ⋙ case _ of
  S.Switch r → M.Just r
  _ → M.Nothing

_Let
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (S.LetR t)
_Let = prism' (embed ∘ S.Let) $ project ⋙ case _ of
  S.Let r → M.Just r
  _ → M.Nothing

_IntLiteral
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t Int
_IntLiteral = prism' (embed ∘ S.Literal ∘ EJ.Integer) $ project ⋙ case _ of
  S.Literal (EJ.Integer r)  → M.Just r
  _ → M.Nothing

_DecimalLiteral
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t HN.HugeNum
_DecimalLiteral = prism' (embed ∘ S.Literal ∘ EJ.Decimal) $ project ⋙ case _ of
  S.Literal (EJ.Decimal r) → M.Just r
  _ → M.Nothing

_StringLiteral
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t String
_StringLiteral = prism' (embed ∘ S.Literal ∘ EJ.String) $ project ⋙ case _ of
  S.Literal (EJ.String r) → M.Just r
  _ → M.Nothing

_NullLiteral
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t Unit
_NullLiteral = prism' (const $ embed $ S.Literal EJ.Null) $ project ⋙ case _ of
  S.Literal EJ.Null → M.Just unit
  _ → M.Nothing

_BoolLiteral
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t Boolean
_BoolLiteral = prism' (embed ∘ S.Literal ∘ EJ.Boolean) $ project ⋙ case _ of
  S.Literal (EJ.Boolean b) → M.Just b
  _ → M.Nothing

_Vari
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t String
_Vari = prism' (embed ∘ S.Vari) $ project ⋙ case _ of
  S.Vari r → M.Just r
  _ → M.Nothing

_Select
  ∷ ∀ t
  . (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF))
  ⇒ Prism' t (S.SelectR t)
_Select = prism' (embed ∘ S.Select) $ project ⋙ case _ of
  S.Select r → M.Just r
  _ → M.Nothing

_Parens ∷ ∀ t. (Recursive t (S.SqlF EJ.EJsonF), Corecursive t (S.SqlF EJ.EJsonF)) ⇒ Prism' t t
_Parens = prism' (embed ∘ S.Parens) $ project ⋙ case _ of
  S.Parens t → M.Just t
  _ → M.Nothing
