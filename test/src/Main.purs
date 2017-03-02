module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)

import Data.List (List)
import Data.Functor.Mu (Mu)
import Debug.Trace (traceAnyA)
import SqlSquare.AST as S
import Matryoshka (class Corecursive, embed, cata)

num ∷ ∀ t. Corecursive t S.AST ⇒ Number → t
num n = embed (S.FloatLiteral n)

invokeFunction ∷ ∀ t. Corecursive t S.AST ⇒ String → List t → t
invokeFunction name args = embed (S.InvokeFunction { name, args })

someExpr ∷ ∀ t. Corecursive t S.AST ⇒ t
someExpr = invokeFunction "foo" $ pure $ num 12.0

main ∷ ∀ e. Eff e Unit
main = do
  traceAnyA (someExpr ∷ Mu S.AST)
  traceAnyA $ cata S.print (someExpr ∷ Mu S.AST)
