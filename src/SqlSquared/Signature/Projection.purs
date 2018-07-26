module SqlSquared.Signature.Projection where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable as F
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String.Gen as GenS
import Data.Traversable as T
import Matryoshka (Algebra, CoalgebraM)
import SqlSquared.Signature.Ident (Ident(..), printIdent)
import SqlSquared.Utils ((∘))

newtype Projection a = Projection { expr ∷ a, alias ∷ Maybe Ident }

derive instance functorProjection ∷ Functor Projection
derive instance newtypeProjection ∷ Newtype (Projection a) _
derive instance eqProjection ∷ Eq a ⇒ Eq (Projection a)
derive instance ordProjection ∷ Ord a ⇒ Ord (Projection a)

instance foldableProjection ∷ F.Foldable Projection where
  foldMap f (Projection { expr }) = f expr
  foldl f a (Projection { expr }) = f a expr
  foldr f a (Projection { expr }) = f expr a

instance traversableProjection ∷ T.Traversable Projection where
  traverse f (Projection { expr, alias }) =
    map (Projection ∘ { expr: _, alias}) $ f expr
  sequence = T.sequenceDefault

printProjection ∷ Algebra Projection String
printProjection (Projection { expr, alias }) = expr <> F.foldMap (\a → " AS " <> printIdent a) alias

genProjection ∷ ∀ m. Gen.MonadGen m ⇒ MonadRec m ⇒ CoalgebraM m Projection Int
genProjection n = do
  alias ← map Ident <$> GenC.genMaybe GenS.genUnicodeString
  pure $ Projection { expr: n - 1, alias }
