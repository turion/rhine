module Util where

-- rhine

import Data.Automaton.Schedule (Yield, runYield)
import FRP.Rhine

runScheduleRhinePure :: (Clock Yield cl, GetClockProxy cl) => Rhine Yield cl a b -> [a] -> [Maybe b]
runScheduleRhinePure rhine = runYield . runRhine rhine

runRhine :: (Clock m cl, GetClockProxy cl, Monad m) => Rhine m cl a b -> [a] -> m [Maybe b]
runRhine rhine input = do
  automaton <- eraseClock rhine
  embed automaton input
