{- |
Provides a clock that ticks at every multiple of a fixed number of milliseconds.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module FRP.Rhine.Clock.Realtime.Millisecond where

-- base
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Control.Concurrent (threadDelay)
import GHC.TypeLits

-- fixed-vector
import Data.Vector.Sized (Vector, fromList)

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Clock.FixedStep
import FRP.Rhine.Schedule
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingBuffer.Util
import FRP.Rhine.ResamplingBuffer.Collect

{- |
A clock ticking every 'n' milliseconds,
in real time.
Since 'n' is in the type signature,
it is ensured that when composing two signals on a 'Millisecond' clock,
they will be driven at the same rate.

The tag of this clock is 'Bool',
where 'True' represents successful realtime,
and 'False' a lag.
-}
newtype Millisecond (n :: Nat) = Millisecond (RescaledClockS IO (FixedStep n) UTCTime Bool)
-- TODO Consider changing the tag to Maybe Double

instance Clock IO (Millisecond n) where
  type Time (Millisecond n) = UTCTime
  type Tag  (Millisecond n) = Bool
  initClock (Millisecond cl) = initClock cl

instance GetClockProxy (Millisecond n)

-- | This implementation measures the time after each tick,
--   and waits for the remaining time until the next tick.
--   If the next tick should already have occurred,
--   the tag is set to 'False', representing a failed real time attempt.
waitClock :: KnownNat n => Millisecond n
waitClock = Millisecond $ RescaledClockS FixedStep $ \_ -> do
  initTime <- getCurrentTime
  let
    runningClock = arrM $ \(n, ()) -> do
      beforeSleep <- getCurrentTime
      let
        diff :: Double
        diff      = realToFrac $ beforeSleep `diffUTCTime` initTime
        remaining = fromInteger $ n * 1000 - round (diff * 1000000)
      threadDelay remaining
      now         <- getCurrentTime -- TODO Test whether this is a performance penalty
      return (now, remaining > 0)
  return (runningClock, initTime)


-- TODO It would be great if this could be directly implemented in terms of downsampleFixedStep
downsampleMillisecond
  :: (KnownNat n, Monad m)
  => ResamplingBuffer m (Millisecond k) (Millisecond (n * k)) a (Vector n a)
downsampleMillisecond = collect >>-^ arr (fromList >>> assumeSize)
  where
    assumeSize = fromMaybe $ error $ unwords
      [ "You are using an incorrectly implemented schedule"
      , "for two Millisecond clocks."
      , "Use a correct schedule like downsampleMillisecond."
      ]
