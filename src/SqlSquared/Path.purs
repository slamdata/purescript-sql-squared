module SqlSquared.Path
  ( AnyFilePath
  , AnyDirPath
  , parseAnyFilePath
  , printAnyFilePath
  , parseAnyDirPath
  , printAnyDirPath
  , genAnyFilePath
  , genAnyDirPath
  ) where

import Prelude
import Data.Either as E
import Data.NonEmpty ((:|))
import Data.Path.Pathy as Pt
import Data.Path.Pathy.Gen as PtGen
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import SqlSquared.Utils ((∘))

type AnyDirPath = E.Either (Pt.AbsDir Pt.Unsandboxed) (Pt.RelDir Pt.Unsandboxed)
type AnyFilePath = E.Either (Pt.AbsFile Pt.Unsandboxed) (Pt.RelFile Pt.Unsandboxed)

printAnyDirPath :: AnyDirPath -> String
printAnyDirPath = E.either Pt.unsafePrintPath Pt.unsafePrintPath

parseAnyDirPath :: forall m. Applicative m => (forall a. String -> m a) -> String -> m AnyDirPath
parseAnyDirPath fail = Pt.parsePath
  (pure ∘ E.Right)
  (pure ∘ E.Left)
  (const $ fail "Expected a directory path")
  (const $ fail "Expected a directory path")

printAnyFilePath :: AnyFilePath -> String
printAnyFilePath = E.either Pt.unsafePrintPath Pt.unsafePrintPath

parseAnyFilePath :: forall m. Applicative m => (forall a. String -> m a) -> String -> m AnyFilePath
parseAnyFilePath fail = Pt.parsePath
  (const $ fail "Expected a file path")
  (const $ fail "Expected a file path")
  (pure ∘ E.Right)
  (pure ∘ E.Left)

genAnyFilePath :: forall m. Gen.MonadGen m => MonadRec m => m AnyFilePath
genAnyFilePath = Gen.oneOf
  $ (E.Left ∘ Pt.unsandbox <$> PtGen.genAbsFilePath)
  :| [E.Right ∘ Pt.unsandbox <$> PtGen.genRelFilePath]

genAnyDirPath :: forall m. Gen.MonadGen m => MonadRec m => m AnyDirPath
genAnyDirPath = Gen.oneOf
  $ (E.Left ∘ Pt.unsandbox <$> PtGen.genAbsDirPath)
  :| [E.Right ∘ Pt.unsandbox <$> PtGen.genRelDirPath]
