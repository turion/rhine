module Util where

-- automaton
import Data.Automaton.Schedule.Trans (Schedule, evalSchedule)

-- rhine
import FRP.Rhine

runScheduleRhinePure :: (Clock (Schedule (Seconds Integer)) cl, GetClockProxy cl) => Rhine (Schedule (Seconds Integer)) cl a b -> [a] -> [Maybe b]
runScheduleRhinePure rhine = evalSchedule . runRhine rhine

runRhine :: (Clock m cl, GetClockProxy cl, Monad m) => Rhine m cl a b -> [a] -> m [Maybe b]
runRhine rhine = embed $ eraseClock rhine
