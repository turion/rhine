{-# LANGUAGE UndecidableInstances #-}

module FRP.Rhine.Clock.Realtime where

-- base
import Control.Arrow (returnA)
import Control.Concurrent (threadDelay)
import Control.Monad (guard)
import Control.Monad.IO.Class

-- time
import Data.Time (addUTCTime, diffUTCTime, getCurrentTime)

-- automaton
import Data.Automaton

-- rhine
import FRP.Rhine.Clock

-- time-domain
import Data.TimeDomain (Diff, UTCTime)
import FRP.Rhine.Clock.Proxy (GetClockProxy)

-- | Use the waiting effect of @cl@, but overwrite the time stamps with the current system time.
newtype OverwriteUTCClock cl = OverwriteUTCClock cl

instance (Clock m cl, MonadIO m) => Clock m (OverwriteUTCClock cl) where
  type Time (OverwriteUTCClock cl) = UTCTime
  type Tag (OverwriteUTCClock cl) = Tag cl
  runClock = proc (OverwriteUTCClock cl) -> do
    (_timePassed, tag) <- runClock -< cl
    time <- constM $ liftIO getCurrentTime -< ()
    returnA -< (time, tag)

{- | Rescale a clock to the UTC time domain.

The initial time stamp is measured as system time,
and the increments (durations between ticks) are taken from the original clock.
No attempt at waiting until the specified time is made,
the timestamps of the original clock are trusted unconditionally.
Th original clock is expected to start running at the time '0'.
-}
newtype AddUTCClock cl = AddUTCClock cl

instance (Clock m cl, Real (Time cl), MonadIO m) => Clock m (AddUTCClock cl) where
  type Time (AddUTCClock cl) = UTCTime
  type Tag (AddUTCClock cl) = Tag cl
  runClock = proc (AddUTCClock cl) -> do
    (timePassed, tag) <- runClock -< cl
    initTime <- constM $ liftIO getCurrentTime -< ()
    returnA -< (addUTCTime (realToFrac timePassed) initTime, tag)

instance GetClockProxy (AddUTCClock cl)

{- | Like 'UTCClock', but also output in the tag whether and by how much the target realtime was missed.

The original clock specifies with its time stamps when, relative to the initialisation time,
the UTC clock should tick.
A tag of @(tag, 'Nothing')@ means that the tick was in time.
@(tag, 'Just' dt)@ means that the tick was too late by @dt@.
-}
newtype WaitUTCClock cl = WaitUTCClock cl

{- | Measure the time after each tick, and wait for the remaining time until the next tick.

If the next tick should already have occurred @dt@ seconds ago,
the tag is set to @'Just' dt@, representing a failed real time attempt.

Note that this clock internally uses 'threadDelay' which can block
for quite a lot longer than the requested time, which can cause
'waitUTC' to miss one or more ticks when using a fast original clock.
When using 'threadDelay', the difference between the real wait time
and the requested wait time will be larger when using
the @-threaded@ ghc option (around 800 microseconds) than when not using
this option (around 100 microseconds). For fast clocks it is recommended
that @-threaded@ not be used in order to miss less ticks. The clock will adjust
the wait time, up to no wait time at all, to catch up when a tick is missed.
-}
instance (Clock m cl, Real (Time cl), MonadIO m, Fractional (Diff (Time cl))) => Clock m (WaitUTCClock cl) where
  type Time (WaitUTCClock cl) = UTCTime
  type Tag (WaitUTCClock cl) = (Tag cl, Maybe (Diff (Time cl)))
  runClock = proc (WaitUTCClock cl) -> do
    (sinceInitTarget, tag) <- runClock -< cl
    initTime <- initialised_ $ liftIO getCurrentTime -< ()
    beforeSleep <- constM $ liftIO getCurrentTime -< ()
    let
      diff :: Rational
      diff = toRational $ beforeSleep `diffUTCTime` initTime
      remaining = toRational sinceInitTarget - diff
    arrM $ liftIO . threadDelay -< round $ 1000000 * realToFrac remaining
    now <- constM $ liftIO getCurrentTime -< ()
    returnA -< (now, (tag, guard (remaining > 0) >> return (fromRational remaining)))

instance GetClockProxy (WaitUTCClock cl)
