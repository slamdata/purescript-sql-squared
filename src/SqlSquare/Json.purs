module SqlSquare.Json where

import Prelude

import Data.Argonaut (JCursor(..))
import Data.Argonaut as JS
import Data.Set as Set
import Data.List as L
import Data.Foldable as F
import Data.Tuple (fst)
import Data.Maybe (Maybe(..))

import SqlSquare.AST as S
import SqlSquare.Utils ((∘), (⋙))

import Matryoshka (ana, Coalgebra)

data UnfoldableJC = JC JCursor | S String | I Int

jcCoalgebra ∷ Coalgebra S.SqlF UnfoldableJC
jcCoalgebra = case _ of
  S s → S.StringLiteral s
  I i → S.IntLiteral i
  JC cursor → case cursor of
    JCursorTop → S.Splice Nothing
    JIndex i c → S.Binop { op: S.IndexDeref, lhs: JC c, rhs: I i }
    JField f c → S.Binop { op: S.FieldDeref, lhs: JC c, rhs: S f }

jcursorToSql ∷ JCursor → S.Sql
jcursorToSql = JC ⋙ ana jcCoalgebra

fields ∷ JS.JArray → L.List S.Sql
fields arr =
  map jcursorToSql $ L.fromFoldable $ F.foldMap (Set.fromFoldable ∘ map fst) $ map JS.toPrims arr
