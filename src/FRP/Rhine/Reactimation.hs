{-# LANGUAGE GADTs #-}
module FRP.Rhine.Reactimation where


-- dunai
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Reactimation.Tick
import FRP.Rhine.Schedule
import FRP.Rhine.SF
import FRP.Rhine.SyncSF


-- | An SF together with a clock of the correct type.
--   A Rhine is a complete reactive program.
data Rhine m cl a b = Rhine
  { sf    :: SF m cl a b
  , clock :: cl
  }

-- * Syntactic sugar to create Rhines

-- | Create a synchronous rhine by combining a synchronous SF with a matching clock
(@@) :: (Monad m
        ,Clock m cl
        , cl ~ Leftmost cl
        , cl ~ Rightmost cl )
     => SyncSF m cl a b -> cl -> Rhine m cl a b
(@@) = Rhine . Synchronous

-- TODO Add the other sugar like -->, >--, -@-

-- * Running a Rhine

-- | Takes a Rhine with no input or output data and runs it.
flow :: ( Monad m
        , Clock m cl
        , TimeDomainOf cl ~ TimeDomainOf (Leftmost  cl)
        , TimeDomainOf cl ~ TimeDomainOf (Rightmost cl)
        )
     => Rhine m cl () () -> m ()
flow rhine = do
  let cl = clock rhine
  -- Create a stream of time stamps
  timeInfo <- genTimeInfo cl
  -- Run the main loop
  flow' timeInfo $ Tickable (trivialResamplingBuffer cl) (sf rhine) (trivialResamplingBuffer cl)
    where
      flow' timeInfo tickable = do
        -- Fetch the next time stamp from the stream, wait if necessary
        (ti, timeInfo') <- unMSF timeInfo ()
        -- Process the part of the signal network that is scheduled to run
        tickable' <- tick tickable ti
        -- Loop
        flow' timeInfo' tickable'
