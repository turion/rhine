{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
module FRP.Rhine.Reactimation where


-- dunai
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Reactimation.Tick
import FRP.Rhine.Schedule
import FRP.Rhine.SF


{- |
An 'SF' together with a clock of matching type 'cl',
A 'Rhine' is a reactive program, possibly with open inputs and outputs.
If the input and output types 'a' and 'b' are both '()',
that is, the 'Rhine' is "closed",
then it is a standalone reactive program
that can be run with the function 'flow'.
-}
data Rhine m cl a b = Rhine
  { sf    :: SF m cl a b
  , clock :: cl
  }


-- * Running a Rhine

{- |
Takes a closed 'Rhine' (with trivial input and output),
and runs it indefinitely.
All input is created, and all output is consumed by means of side effects
in a monad 'm'.

Basic usage (synchronous case):

@
sensor :: ClSF MyMonad MyClock () a
sensor = arrMSync_ produceData

processing :: ClSF MyMonad MyClock a b
processing = ...

actuator :: ClSF MyMonad MyClock b ()
actuator = arrMSync consumeData

mainSF :: ClSF MyMonad MyClock () ()
mainSF = sensor >-> processing >-> actuator

main :: MyMonad ()
main = flow $ mainSF @@ clock
@
-}
-- TODO Can we chuck the constraints into Clock m cl?
flow
  :: ( Monad m, Clock m cl
     , Time cl ~ Time (Leftmost  cl)
     , Time cl ~ Time (Rightmost cl)
     )
  => Rhine m cl () () -> m ()
flow Rhine {..} = do
  (runningClock, initTime) <- startClock clock
  -- Run the main loop
  flow' runningClock $ createTickable
    (trivialResamplingBuffer clock)
    sf
    (trivialResamplingBuffer clock)
    initTime
    where
      flow' runningClock tickable = do
        -- Fetch the next time stamp from the stream, wait if necessary
        ((now, tag), runningClock') <- unMSF runningClock ()
        -- Process the part of the signal network that is scheduled to run
        tickable' <- tick tickable now tag
        -- Loop
        flow' runningClock' tickable'
