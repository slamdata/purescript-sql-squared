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

import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either as E
import Data.NonEmpty ((:|))
import Pathy (posixParser, posixPrinter)
import Pathy as Pt
import Pathy.Gen as PtGen
import SqlSquared.Utils ((∘))

type AnyDirPath = E.Either Pt.AbsDir Pt.RelDir
type AnyFilePath = E.Either Pt.AbsFile Pt.RelFile

printAnyDirPath :: AnyDirPath -> String
printAnyDirPath = E.either
  (Pt.sandboxAny >>> Pt.unsafePrintPath posixPrinter)
  (Pt.sandboxAny >>> Pt.unsafePrintPath posixPrinter)

parseAnyDirPath :: forall m. Applicative m => (forall a. String -> m a) -> String -> m AnyDirPath
parseAnyDirPath fail = Pt.parsePath posixParser
  (pure ∘ E.Right)
  (pure ∘ E.Left)
  (const $ fail "Expected a directory path")
  (const $ fail "Expected a directory path")
  (fail "Expected valid path")

printAnyFilePath :: AnyFilePath -> String
printAnyFilePath = E.either
  (Pt.sandboxAny >>> Pt.unsafePrintPath posixPrinter)
  (Pt.sandboxAny >>> Pt.unsafePrintPath posixPrinter)

parseAnyFilePath :: forall m. Applicative m => (forall a. String -> m a) -> String -> m AnyFilePath
parseAnyFilePath fail = Pt.parsePath posixParser
  (const $ fail "Expected a file path")
  (const $ fail "Expected a file path")
  (pure ∘ E.Right)
  (pure ∘ E.Left)
  (fail "Expected valid path")

genAnyFilePath :: forall m. Gen.MonadGen m => MonadRec m => m AnyFilePath
genAnyFilePath = Gen.oneOf
  $ (E.Left <$> PtGen.genAbsFilePath)
  :| [E.Right <$> PtGen.genRelFilePath]

genAnyDirPath :: forall m. Gen.MonadGen m => MonadRec m => m AnyDirPath
genAnyDirPath = Gen.oneOf
  $ (E.Left <$> PtGen.genAbsDirPath)
  :| [E.Right <$> PtGen.genRelDirPath]
