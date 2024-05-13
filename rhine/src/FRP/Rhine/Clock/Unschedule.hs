{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A clock that removes the 'ScheduleT' transformer from the stack by interpreting its actions in a monad
module FRP.Rhine.Clock.Unschedule where

-- base
import Control.Arrow
import Control.Concurrent qualified as Concurrent (yield)
import Control.Monad.IO.Class

-- monad-schedule
import Control.Monad.Schedule.Trans

-- automaton
import Data.Automaton (hoistS)

-- rhine
import FRP.Rhine.Clock

{- | If @cl@ is a 'Clock' in 'ScheduleT diff m', apply 'UnscheduleClock'
  to get a clock in 'm'.
-}
data UnscheduleClock m cl = UnscheduleClock
  { scheduleClock :: cl
  , scheduleWait :: Diff (Time cl) -> m ()
  }

{- | Remove a 'ScheduleT' layer from the monad transformer stack of the clock.

The 'yield' action is interpreted as thread yielding in 'IO'.
-}
unyieldClock :: cl -> UnscheduleClock IO cl
unyieldClock cl = UnscheduleClock cl $ const $ liftIO Concurrent.yield

instance (TimeDomain (Time cl), Clock (ScheduleT (Diff (Time cl)) m) cl, Monad m) => Clock m (UnscheduleClock m cl) where
  type Tag (UnscheduleClock _ cl) = Tag cl
  type Time (UnscheduleClock _ cl) = Time cl
  initClock UnscheduleClock {scheduleClock, scheduleWait} = run $ first (hoistS run) <$> initClock scheduleClock
    where
      run :: ScheduleT (Diff (Time cl)) m a -> m a
      run = runScheduleT scheduleWait
