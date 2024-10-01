module FRP.Rhine.Clock.Realtime where

-- base
import Control.Arrow (arr)
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

{- | A clock rescaled to the 'UTCTime' time domain.

There are different strategies how a clock may be rescaled, see below.
-}
type UTCClock m cl = RescaledClockS m cl UTCTime (Tag cl)

-- | Rescale an 'IO' clock to the UTC time domain, overwriting its timestamps.
overwriteUTC :: (MonadIO m) => cl -> UTCClock m cl
overwriteUTC cl =
  RescaledClockS
    { unscaledClockS = cl
    , rescaleS = const $ do
        now <- liftIO getCurrentTime
        return (arrM $ \(_timePassed, tag) -> (,tag) <$> liftIO getCurrentTime, now)
    }

{- | Rescale a clock to the UTC time domain.

The initial time stamp is measured as system time,
and the increments (durations between ticks) are taken from the original clock.
No attempt at waiting until the specified time is made,
the timestamps of the original clock are trusted unconditionally.
-}
addUTC :: (Real (Time cl), MonadIO m) => cl -> UTCClock m cl
addUTC cl =
  RescaledClockS
    { unscaledClockS = cl
    , rescaleS = const $ do
        now <- liftIO getCurrentTime
        return (arr $ \(timePassed, tag) -> (addUTCTime (realToFrac timePassed) now, tag), now)
    }

{- | Like 'UTCClock', but also output in the tag whether and by how much the target realtime was missed.

The original clock specifies with its time stamps when, relative to the initialisation time,
the UTC clock should tick.
A tag of @(tag, 'Nothing')@ means that the tick was in time.
@(tag, 'Just' dt)@ means that the tick was too late by @dt@.
-}
type WaitUTCClock m cl = RescaledClockS m cl UTCTime (Tag cl, Maybe (Diff (Time cl)))

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
waitUTC :: (Real (Time cl), MonadIO m, Fractional (Diff (Time cl))) => cl -> WaitUTCClock m cl
waitUTC unscaledClockS =
  RescaledClockS
    { unscaledClockS
    , rescaleS = \_ -> do
        initTime <- liftIO getCurrentTime
        let
          runningClock = arrM $ \(sinceInitTarget, tag) -> liftIO $ do
            beforeSleep <- getCurrentTime
            let
              diff :: Rational
              diff = toRational $ beforeSleep `diffUTCTime` initTime
              remaining = toRational sinceInitTarget - diff
            threadDelay $ round $ 1000000 * remaining
            now <- getCurrentTime
            return (now, (tag, guard (remaining > 0) >> return (fromRational remaining)))
        return (runningClock, initTime)
    }
{-# INLINE waitUTC #-}
