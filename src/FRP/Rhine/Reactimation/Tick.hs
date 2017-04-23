{-# LANGUAGE GADTs #-}
module FRP.Rhine.Reactimation.Tick where

-- transformers
import Control.Monad.Trans.Reader


-- dunai
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Schedule
import FRP.Rhine.SF

data Tickable m clab cl clcd a b c d = Tickable
  { buffer1 :: ResamplingBuffer m  clab          (Leftmost cl) a b
  , ticksf  :: SF m cl b c
  , buffer2 :: ResamplingBuffer m (Rightmost cl) clcd          c d
  }
-- TODO It's a ReactHandle!
-- TODO Ponder whether ticksf is a good name or whether Tickable belongs in a different module

-- | In this function, one tick of an asynchronous signal function happens.
--   The TimeInfo holds the information which part of the signal tree will tick.
--   This information is encoded in the Tag of the TimeInfo,
--   which is of type Either (Tag clab) (Tag clcd) in case of a CombinedClock,
--   encoding either a tick for the left clock or the right clock.
tick :: ( Monad m
        , TimeDomainOf clab ~ TimeDomainOf cl
        , TimeDomainOf clcd ~ TimeDomainOf cl
        , TimeDomainOf (Leftmost cl) ~ TimeDomainOf cl
        , TimeDomainOf (Rightmost cl) ~ TimeDomainOf cl
        )
     => Tickable m clab cl clcd a b c d -> TimeInfo cl
     -> m (Tickable m clab cl clcd a b c d)
-- Only if we have reached a leaf of the tree, data is actually processed.
tick (Tickable buf1 (Synchronous syncsf) buf2) ti = do
  -- Get an input value from the left buffer
  (b, buf1'  ) <- get buf1 ti
  -- Run it through the synchronous signal function
  (c, syncsf') <- unMSF syncsf b `runReaderT` ti
  -- Put the output into the right buffer
  buf2'        <- put buf2 ti c
  return $ Tickable buf1' (Synchronous syncsf') buf2'
-- In case of a CombinedClock, asynchronous resampling happens.
-- The tag of the TimeInfo decides on which side data is processed.
-- The other side is left unchanged.
tick (Tickable buf1 (Resampling sf1 buf sf2) buf2) ti = case tag ti of
  Left  tag1 -> do
    left <- tick (Tickable buf1 sf1 buf)
      $ TimeInfo
        (sinceTick  ti)
        (sinceStart ti)
        (absolute   ti)
        tag1
    return $ Tickable (buffer1 left) (Resampling (ticksf left) (buffer2 left) sf2) buf2
  Right tag2 -> do
    right <- tick (Tickable buf sf2 buf2)
      $ TimeInfo
        (sinceTick  ti)
        (sinceStart ti)
        (absolute   ti)
        tag2
    return $ Tickable buf1 (Resampling sf1 (buffer1 right) (ticksf right)) (buffer2 right)
