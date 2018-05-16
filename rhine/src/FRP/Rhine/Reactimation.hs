{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module FRP.Rhine.Reactimation where


-- transformers
import Control.Monad.Trans.Class

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
sensor :: SyncSF MyMonad MyClock () a
sensor = arrMSync_ produceData

processing :: SyncSF MyMonad MyClock a b
processing = ...

actuator :: SyncSF MyMonad MyClock b ()
actuator = arrMSync consumeData

mainSF :: SyncSF MyMonad MyClock () ()
mainSF = sensor >-> processing >-> actuator

main :: MyMonad ()
main = flow $ mainSF @@ clock
@
-}
-- TODO Can we chuck the constraints into Clock m cl?
flow
  :: ( Monad m, Clock m cl
     , TimeDomainOf cl ~ TimeDomainOf (Leftmost  cl)
     , TimeDomainOf cl ~ TimeDomainOf (Rightmost cl)
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

-- * Hoist 'Rhine's along monad morphisms

hoistSeqRhine
  :: ( Monad m, Monad m'
     , cl1 ~ Leftmost cl1, cl1 ~ Rightmost cl1
     , cl2 ~ Leftmost cl2, cl2 ~ Rightmost cl2
     )
  => (forall x . m x -> m' x)
  -> Rhine m (SequentialClock m cl1 cl2) a b
  -> Rhine m' (SequentialClock m' (HoistClock m m' cl1) (HoistClock m m' cl2)) a b
hoistSeqRhine monadMorphism Rhine {..} = Rhine
  { sf    = hoistSeqSF monadMorphism sf
  , clock = hoistedSeqClock monadMorphism clock
  }

liftSeqRhine
  :: ( Monad m, MonadTrans t, Monad (t m)
     , cl1 ~ Leftmost cl1, cl1 ~ Rightmost cl1
     , cl2 ~ Leftmost cl2, cl2 ~ Rightmost cl2
     )
  => Rhine m (SequentialClock m cl1 cl2) a b
  -> Rhine (t m) (SequentialClock (t m) (LiftClock m t cl1) (LiftClock m t cl2)) a b
liftSeqRhine = hoistSeqRhine lift
