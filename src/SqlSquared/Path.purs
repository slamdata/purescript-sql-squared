module SqlSquared.Path
  ( parseAnyFilePath
  , printAnyFilePath
  , parseAnyDirPath
  , printAnyDirPath
  , genAnyFilePath
  , genAnyDirPath
  , module PathyTypeReexprts
  ) where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either as E
import Data.NonEmpty ((:|))
import Pathy as Pt
import Pathy (AnyDir, AnyFile) as PathyTypeReexprts
import Pathy (AnyDir, AnyFile)
import Pathy.Gen as PtGen
import SqlSquared.Utils ((∘))


printAnyDirPath :: AnyDir -> String
printAnyDirPath = E.either
  (Pt.sandboxAny >>> Pt.unsafePrintPath Pt.posixPrinter)
  (Pt.sandboxAny >>> Pt.unsafePrintPath Pt.posixPrinter)

parseAnyDirPath :: forall m. Applicative m => (forall a. String -> m a) -> String -> m AnyDir
parseAnyDirPath fail = Pt.parsePath Pt.posixParser
  (pure ∘ E.Right)
  (pure ∘ E.Left)
  (const $ fail "Expected a directory path")
  (const $ fail "Expected a directory path")
  (fail "Expected valid path")

printAnyFilePath :: AnyFile -> String
printAnyFilePath = E.either
  (Pt.sandboxAny >>> Pt.unsafePrintPath Pt.posixPrinter)
  (Pt.sandboxAny >>> Pt.unsafePrintPath Pt.posixPrinter)

parseAnyFilePath :: forall m. Applicative m => (forall a. String -> m a) -> String -> m AnyFile
parseAnyFilePath fail = Pt.parsePath Pt.posixParser
  (const $ fail "Expected a file path")
  (const $ fail "Expected a file path")
  (pure ∘ E.Right)
  (pure ∘ E.Left)
  (fail "Expected valid path")

genAnyFilePath :: forall m. Gen.MonadGen m => MonadRec m => m AnyFile
genAnyFilePath = Gen.oneOf
  $ (E.Left <$> PtGen.genAbsFilePath)
  :| [E.Right <$> PtGen.genRelFilePath]

genAnyDirPath :: forall m. Gen.MonadGen m => MonadRec m => m AnyDir
genAnyDirPath = Gen.oneOf
  $ (E.Left <$> PtGen.genAbsDirPath)
  :| [E.Right <$> PtGen.genRelDirPath]
