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
import Data.Csv

-- rhine
import FRP.Rhine (GetClockProxy)
import FRP.Rhine.Clock
import FRP.Rhine.Clock.File

{- | Similar to 'File', but ticks at every line of a CSV file,
and returns a parsed record.

Additionally to an end-of-file error, it can also return a parse error as a 'String'.
-}
newtype CsvClock = CsvClock (File String (Vector Record))

-- | Create a 'CsvClock' from a file path.
csvClock :: FilePath -> CsvClock
csvClock filename =
  CsvClock
    File
      { action = fmap (decode NoHeader . fromStrict) . Data.ByteString.hGetLine
      , filename
      }

instance Clock (ExceptT (Either String FileException) IO) CsvClock where
  type Time CsvClock = Integer
  type Tag CsvClock = Vector Record
  initClock (CsvClock fileClock) = initClock fileClock

instance GetClockProxy CsvClock
