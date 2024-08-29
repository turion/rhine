module FRP.Rhine.Clock.CsvLazy where

-- transformers
import Control.Monad.Trans.Except

-- bytestring
import Data.ByteString

-- vector
import Data.Vector

-- cassava
import Data.Csv (Record)
import Data.Csv.Incremental

-- dunai
import Data.Automaton.Trans.Except (throwS)

-- rhine

import Data.Either (fromRight)
import Data.Stream.Async (concatS)
import Data.Void
import FRP.Rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.File
import GHC.IO.Handle (hIsEOF)
import GHC.IO.Exception (IOErrorType(EOF))
import Data.ByteString.Lazy
import Data.Csv.Streaming (decode, Records (..))
import Data.Maybe (fromMaybe)
import qualified FRP.Rhine.Clock.Csv as FRP.Rhine.Clock


newtype CsvClockLazy = CsvClockLazy { getCsvClockLazy :: FilePath }

instance MonadIO m => Clock (ExceptT (Either String FileException) m) CsvClockLazy where
  type Time CsvClockLazy = Integer
  type Tag CsvClockLazy = Record

  initClock CsvClockLazy { getCsvClockLazy } = do
    stream <- liftIO $ Data.ByteString.Lazy.readFile getCsvClockLazy
    let initialRecords = Data.Csv.Streaming.decode NoHeader stream
        runningClock = feedback initialRecords $ proc ((), records) -> do
          n <- FRP.Rhine.Clock.count -< ()
          case records of
            Cons (Left e) _records' ->
              Data.Automaton.Trans.Except.throwS -< Left e
            Cons (Right a) records' ->
               returnA -< ((n, a), records')
            Nil ms _bs ->
              Data.Automaton.Trans.Except.throwS -< Left $ fromMaybe "CsvClockLazy: Unknown CSV decoding error" ms
    return (runningClock, 0)

instance GetClockProxy CsvClockLazy
