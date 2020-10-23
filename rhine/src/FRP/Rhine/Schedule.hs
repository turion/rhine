{- |
'Schedule's are the compatibility mechanism between two different clocks.
A schedule' implements the the universal clocks such that those two given clocks
are its subclocks.

This module defines the 'Schedule' type and certain general constructions of schedules,
such as lifting along monad morphisms or time domain morphisms.
It also supplies (sequential and parallel) compositions of clocks.

Specific implementations of schedules are found in submodules.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.Schedule where

-- base
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as N
import Data.Semigroup

-- dunai
import Data.MonadicStreamFunction.InternalCore
import Data.MonadicStreamFunction

-- rhine
import Control.Monad.Schedule.Class
import FRP.Rhine.Clock

scheduleList :: (Monad m, MonadSchedule m) => NonEmpty (MSF m a b) -> MSF m a b
scheduleList msfs = scheduleList' msfs []
  where
    scheduleList' msfs running = MSF $ \a -> do
      let bsAndConts = flip unMSF a <$> msfs
      ((b, msf'), bsmsfs) <- schedule $ (N.head bsAndConts :| N.tail bsAndConts ++ running)
      return (b, scheduleList' (msf' :| []) bsmsfs)

-- * The schedule type

-- | Two clocks in the 'ScheduleT' monad transformer
--   can always be canonically scheduled.
--   Indeed, this is the purpose for which 'ScheduleT' was defined.
runningSchedule
  :: ( Monad m, MonadSchedule m
     , Clock m cl1
     , Clock m cl2
     , Time cl1 ~ Time cl2
     )
  => cl1 -> cl2
  -> MSF m () (Time cl1, Tag cl1)
  -> MSF m () (Time cl2, Tag cl2)
  -> MSF m () (Time cl1, Either (Tag cl1) (Tag cl2))
runningSchedule cl1 cl2 rc1 rc2 = scheduleList $ [rc1 >>> arr (second Left), rc2 >>> arr (second Right)]

-- | A schedule implements a combination of two clocks.
--   It outputs a time stamp and an 'Either' value,
--   which specifies which of the two subclocks has ticked.
initSchedule
  :: ( Time cl1 ~ Time cl2
     , Monad m, MonadSchedule m
     , Clock m cl1, Clock m cl2
     )
  => cl1 -> cl2
  -> RunningClockInit m (Time cl1) (Either (Tag cl1) (Tag cl2))
initSchedule cl1 cl2 = do
  (runningClock1, initTime) <- initClock cl1
  (runningClock2, _)        <- initClock cl2
  return
    ( runningSchedule cl1 cl2 runningClock1 runningClock2
    , initTime
    )

-- * Composite clocks

-- ** Sequentially combined clocks

-- | Two clocks can be combined with a schedule as a clock
--   for an asynchronous sequential composition of signal networks.
data SequentialClock cl1 cl2
  = Time cl1 ~ Time cl2
  => SequentialClock
    { sequentialCl1      :: cl1
    , sequentialCl2      :: cl2
    }

-- | Abbrevation synonym.
type SeqClock cl1 cl2 = SequentialClock cl1 cl2

instance (Monad m, MonadSchedule m, Clock m cl1, Clock m cl2)
      => Clock m (SequentialClock cl1 cl2) where
  type Time (SequentialClock cl1 cl2) = Time cl1
  type Tag  (SequentialClock cl1 cl2) = Either (Tag cl1) (Tag cl2)
  initClock SequentialClock {..}
    = initSchedule sequentialCl1 sequentialCl2

-- ** Parallelly combined clocks

-- | Two clocks can be combined with a schedule as a clock
--   for an asynchronous parallel composition of signal networks.
data ParallelClock cl1 cl2
  = Time cl1 ~ Time cl2
  => ParallelClock
    { parallelCl1      :: cl1
    , parallelCl2      :: cl2
    }

-- | Abbrevation synonym.
type ParClock cl1 cl2 = ParallelClock cl1 cl2

instance (Monad m, MonadSchedule m, Clock m cl1, Clock m cl2)
      => Clock m (ParallelClock cl1 cl2) where
  type Time (ParallelClock cl1 cl2) = Time cl1
  type Tag  (ParallelClock cl1 cl2) = Either (Tag cl1) (Tag cl2)
  initClock ParallelClock {..}
    = initSchedule parallelCl1 parallelCl2


-- * Navigating the clock tree

-- | The clock that represents the rate at which data enters the system.
type family In cl where
  In (SequentialClock cl1 cl2) = In cl1
  In (ParallelClock   cl1 cl2) = ParallelClock (In cl1) (In cl2)
  In cl                          = cl

-- | The clock that represents the rate at which data leaves the system.
type family Out cl where
  Out (SequentialClock cl1 cl2) = Out cl2
  Out (ParallelClock   cl1 cl2) = ParallelClock (Out cl1) (Out cl2)
  Out cl                          = cl


-- | A tree representing possible last times to which
--   the constituents of a clock may have ticked.
data LastTime cl where
  SequentialLastTime
    :: LastTime cl1 -> LastTime cl2
    -> LastTime (SequentialClock cl1 cl2)
  ParallelLastTime
    :: LastTime cl1 -> LastTime cl2
    -> LastTime (ParallelClock   cl1 cl2)
  LeafLastTime :: Time cl -> LastTime cl


-- | An inclusion of a clock into a tree of parallel compositions of clocks.
data ParClockInclusion clS cl where
  ParClockInL
    :: ParClockInclusion (ParallelClock clL clR) cl
    -> ParClockInclusion                clL      cl
  ParClockInR
    :: ParClockInclusion (ParallelClock clL clR) cl
    -> ParClockInclusion                      clR  cl
  ParClockRefl :: ParClockInclusion cl cl

-- | Generates a tag for the composite clock from a tag of a leaf clock,
--   given a parallel clock inclusion.
parClockTagInclusion :: ParClockInclusion clS cl -> Tag clS -> Tag cl
parClockTagInclusion (ParClockInL parClockInL) tag = parClockTagInclusion parClockInL $ Left  tag
parClockTagInclusion (ParClockInR parClockInR) tag = parClockTagInclusion parClockInR $ Right tag
parClockTagInclusion ParClockRefl              tag = tag
