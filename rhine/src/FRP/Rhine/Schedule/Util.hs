-- | Utility to define certain deterministic schedules.

module FRP.Rhine.Schedule.Util where

-- dunai
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.Async

-- | In a composite running clock,
--   duplicate the tick of one subclock.
duplicateSubtick :: Monad m => MSF m () (time, Either a b) -> MSF m () (time, Either a (Either a b))
duplicateSubtick runningClock = concatS $ runningClock >>> arr duplicateLeft
  where
    duplicateLeft (time, Left a)  = [(time, Left a), (time, Right $ Left a)]
    duplicateLeft (time, Right b) = [(time, Right $ Right b)]

-- TODO Why is stuff like this not in base? Maybe send pull request...
swapEither :: Either a b -> Either b a
swapEither (Left  a) = Right a
swapEither (Right b) = Left  b
