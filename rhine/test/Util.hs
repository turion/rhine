module Util where

-- monad-schedule
import Control.Monad.Schedule.Trans (Schedule, runScheduleT)

-- rhine

import Data.Functor.Identity (Identity (runIdentity))
import FRP.Rhine

runScheduleRhinePure :: (Clock (Schedule (Diff (Time cl))) cl, GetClockProxy cl) => Rhine (Schedule (Diff (Time cl))) cl a b -> [a] -> [Maybe b]
runScheduleRhinePure rhine = runSchedule . runRhine rhine

runRhine :: (Clock m cl, GetClockProxy cl, Monad m) => Rhine m cl a b -> [a] -> m [Maybe b]
runRhine rhine input = do
  msf <- eraseClock rhine
  embed msf input

-- FIXME Move to monad-schedule
runSchedule :: Schedule diff a -> a
runSchedule = runIdentity . runScheduleT (const (pure ()))
