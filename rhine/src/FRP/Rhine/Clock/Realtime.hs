module FRP.Rhine.Clock.Realtime where

-- base
import Control.Arrow (arr, returnA)
import Control.Monad.IO.Class

-- time
import Data.Time (diffUTCTime, getCurrentTime)

-- automaton

-- rhine
import FRP.Rhine.Clock

-- time-domain
import Data.TimeDomain (Diff, UTCTime, TimeDomain (addTime), diffTime)
import Data.Automaton (sumS, mappendS, sumN)
import Control.Monad (guard)

{- | A clock rescaled to the 'UTCTime' time domain.

There are different strategies how a clock may be rescaled, see below.
-}
type UTCClock m cl = RescaledClockM m cl UTCTime

{- | Rescale an 'IO' clock to the UTC time domain, overwriting its timestamps.

It's the unscaled clock's responsibility to produce the waiting effect.
-}
overwriteUTC :: (MonadIO m) => cl -> (UTCTime -> Time cl) -> UTCClock m cl
overwriteUTC cl rescale =
  RescaledClockM
    { unscaledClockM = cl
    , rescaleTimeM = pure . rescale
    , rescaleDiffTimeM = const $ return 0
    }

{- | Rescale a clock to the UTC time domain.

The initial time stamp is measured as system time,
and the increments (durations between ticks) are taken from the original clock.
No attempt at waiting until the specified time is made,
the timestamps of the original clock are trusted unconditionally.
-}
addUTC :: (Real (Diff (Time cl)), MonadIO m) => cl -> UTCClock m cl
addUTC cl =
  RescaledClockM
    { unscaledClockM = cl
    -- FIXME maybe a rescaled clock that records the initial time?
    , rescaleTimeM = _
    --  const $ do
    --     now <- liftIO getCurrentTime
    --     return (arr $ \(timePassed, tag) -> (addUTCTime (realToFrac timePassed) now, tag), now)
    , rescaleDiffTimeM = return . realToFrac
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
waitUTC :: (Fractional (Time cl), MonadIO m, Real (Diff (Time cl)), Fractional (Diff (Time cl))) => cl -> WaitUTCClock m cl
waitUTC unscaledClockS =
  RescaledClockS
    { unscaledClockS
    , rescaleSTime = arr $ \(initTime, time) -> realToFrac $ time `diffUTCTime` initTime
    , rescaleSDiffTime = proc (initTime, lastTime, dTime, tag) -> do
        let diffTimeRescaled = realToFrac dTime -- FIXME Is this really a good function to use
        targetTimeDiff <- sumN -< diffTimeRescaled
        let targetTime = addTime initTime targetTimeDiff
            waitTime = targetTime `diffTime` lastTime
        returnA -< (waitTime, (tag, guard (waitTime < 0) >> return (realToFrac waitTime)))
            -- let
            --   diff :: Rational
            --   diff = toRational $ beforeSleep `diffUTCTime` initTime
            --   remaining = toRational initTime - diff
            -- threadDelay $ round $ 1000000 * remaining
            -- now <- getCurrentTime
            -- return _ -- (now, (tag, guard (remaining > 0) >> return (fromRational remaining)))
    }
