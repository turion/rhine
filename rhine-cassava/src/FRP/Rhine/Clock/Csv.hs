module FRP.Rhine.Clock.Csv (
  CsvClock,
  csvClock,
)
where

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
import Control.Monad.Trans.MSF.Except (throwS)

-- rhine

import Data.Either (fromRight)
import Data.MonadicStreamFunction.Async (concatS)
import Data.Void
import FRP.Rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.File
import GHC.IO.Handle (hIsEOF)
import GHC.IO.Exception (IOErrorType(EOF))

{- | Similar to 'File', but ticks at every line of a CSV file,
and returns a parsed record.

Additionally to an end-of-file error, it can also return a parse error as a 'String'.
-}
newtype CsvClock = CsvClock (File String ByteString) -- FIXME should be Void

-- FIXME Csv should rather be an adapter to an existing clock, e.g. Stdin, or something from the internet

-- | Create a 'CsvClock' from a file path.
csvClock :: FilePath -> CsvClock
csvClock filename =
  CsvClock
    File
      { action = \handle -> do
          isEOF <- hIsEOF handle
          if isEOF
            then return $ Left "EOF"
            else Right <$> Data.ByteString.hGetSome handle 4096
      , filename
      }

-- FIXME Allow headers, or more generally different configs from cassava

instance MonadIO m => Clock (ExceptT (Either String FileException) m) CsvClock where
  type Time CsvClock = Integer
  type Tag CsvClock = Record
  initClock (CsvClock fileClock) = do
    (runningFileClock, initialTime) <- initClock fileClock
    let runningClock = concatS $ feedback Nothing $ proc ((), parser) -> do
          (_lineNr, nextLine) <- runningFileClock -< ()
          case parser of
            Nothing -> do
              case decode NoHeader of
                Many [] parser' -> do
                  case parser' nextLine of
                    Many records parser'' -> do
                      -- arrM $ liftIO . print -< ("initing", records)
                      returnA -< (fromRight (error "CsvClock Internal error in init") <$> records, Just parser'')
                _ -> Control.Monad.Trans.MSF.Except.throwS -< Left "oh no"
            Just f -> case f nextLine of
              (Done leftover) -> Control.Monad.Trans.MSF.Except.throwS -< Left $ show $ Prelude.length leftover
              -- FIXME the error should be escalated
              (Many records parser') -> do
                -- arrM $ liftIO . print -< records
                returnA -< (fromRight (error "CsvClock Internal error") <$> records, Just parser')
              (Fail a msg) -> Control.Monad.Trans.MSF.Except.throwS -< Left msg
    return (FRP.Rhine.Clock.count &&& runningClock, initialTime)

instance GetClockProxy CsvClock
