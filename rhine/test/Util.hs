module Util where

-- base
import Data.Functor.Identity (Identity (runIdentity))

-- monad-schedule
import Control.Monad.Schedule.Trans (Schedule, runScheduleT)

-- rhine
import FRP.Rhine
import Control.Monad.Schedule.Class (MonadSchedule)

runScheduleRhinePure :: (Ord (Diff td), TimeDifference (Diff td)) => Rhine (Schedule (Diff td)) td cls a b -> [a] -> [b]
runScheduleRhinePure rhine = runSchedule . runRhine rhine

runRhine :: (Monad m, MonadSchedule m) => Rhine m td cls a b -> [a] -> m [b]
runRhine rhine = embed $ eraseClock rhine

-- FIXME Move to monad-schedule
runSchedule :: Schedule diff a -> a
runSchedule = runIdentity . runScheduleT (const (pure ()))
