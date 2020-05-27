{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Provides a clock that ticks at every multiple of a fixed number of milliseconds.
-}
module FRP.Rhine.Clock.Realtime.Millisecond where

-- base

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import GHC.TypeLits

-- vector-sized
import Data.Vector.Sized (Vector, fromList)

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.FixedStep
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Clock.Unschedule
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingBuffer.Collect
import FRP.Rhine.ResamplingBuffer.Util

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
newtype Millisecond (n :: Nat) = Millisecond (RescaledClockS IO (UnscheduleClock IO (FixedStep n)) UTCTime Bool)

-- TODO Consider changing the tag to Maybe Double

instance Clock IO (Millisecond n) where
  type Time (Millisecond n) = UTCTime
  type Tag (Millisecond n) = Bool
  initClock (Millisecond cl) = initClock cl

instance GetClockProxy (Millisecond n)

{- | This implementation measures the time after each tick,
   and waits for the remaining time until the next tick.
   If the next tick should already have occurred,
   the tag is set to 'False', representing a failed real time attempt.

   Note that this clock internally uses 'threadDelay' which can block
   for quite a lot longer than the requested time, which can cause
   the clock to miss one or more ticks when using low values of 'n'.
   When using 'threadDelay', the difference between the real wait time
   and the requested wait time will be larger when using
   the '-threaded' ghc option (around 800 microseconds) than when not using
   this option (around 100 microseconds). For low values of @n@ it is recommended
   that '-threaded' not be used in order to miss less ticks. The clock will adjust
   the wait time, up to no wait time at all, to catch up when a tick is missed.
-}
waitClock :: KnownNat n => Millisecond n
waitClock = Millisecond $ RescaledClockS (unyieldClock FixedStep) $ \_ -> do
  initTime <- liftIO getCurrentTime
  let
    runningClock = arrM $ \(n, ()) -> liftIO $ do
      beforeSleep <- getCurrentTime
      let
        diff :: Double
        diff = realToFrac $ beforeSleep `diffUTCTime` initTime
        remaining = fromInteger $ n * 1000 - round (diff * 1000000)
      threadDelay remaining
      now <- getCurrentTime -- TODO Test whether this is a performance penalty
      return (now, remaining > 0)
  return (runningClock, initTime)

{-
-- TODO It would be great if this could be directly implemented in terms of downsampleFixedStep
downsampleMillisecond ::
  (KnownNat n, Monad m) =>
  ResamplingBuffer m (Millisecond k) (Millisecond (n * k)) a (Vector n a)
downsampleMillisecond = collect >>-^ arr (fromList >>> assumeSize)
  where
    assumeSize =
      fromMaybe $
        error "downsampleMillisecond: Internal error. Please report this as a bug: https://github.com/turion/rhine/issues"
