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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.Schedule where

-- transformers
import Control.Monad.Trans.Reader

-- dunai
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.Clock

-- * The schedule type

-- | A schedule implements a combination of two clocks.
--   It outputs a time stamp and an 'Either' value,
--   which specifies which of the two subclocks has ticked.
data Schedule m cl1 cl2
  = (Time cl1 ~ Time cl2)
  => Schedule
    { initSchedule
        :: cl1 -> cl2
        -> RunningClockInit m (Time cl1) (Either (Tag cl1) (Tag cl2))
    }
-- The type constraint in the constructor is actually useful when pattern matching on 'Schedule',
-- which is interesting since a constraint like 'Monad m' is useful.
-- When reformulating as a GADT, it might get used,
-- but that would mean that we can't use record syntax.


-- * Utilities to create new schedules from existing ones

-- | Lift a schedule along a monad morphism.
hoistSchedule
  :: (Monad m1, Monad m2)
  => (forall a . m1 a -> m2 a)
  -> Schedule m1 cl1 cl2
  -> Schedule m2 cl1 cl2
hoistSchedule hoist Schedule {..} = Schedule initSchedule'
  where
    initSchedule' cl1 cl2 = hoist
      $ first (hoistMSF hoist) <$> initSchedule cl1 cl2
    hoistMSF = liftMSFPurer
    -- TODO This should be a dunai issue


hoistClockSchedule
  :: ( Monad m1, Monad m2
     , Time cl1 ~ Time cl2)
  => (forall a . m1 a -> m2 a)
  -> Schedule m1 cl1 cl2
  -> Schedule m2 (HoistClock m1 m2 cl1) (HoistClock m1 m2 cl2)
hoistClockSchedule hoist schedule = Schedule initSchedule'
  where
    initSchedule' (HoistClock cl1 _) (HoistClock cl2 _)
      = initSchedule (hoistSchedule hoist schedule) cl1 cl2

-- | Swaps the clocks for a given schedule.
flipSchedule
  :: Monad m
  => Schedule m cl1 cl2
  -> Schedule m cl2 cl1
flipSchedule Schedule {..} = Schedule initSchedule_
  where
    initSchedule_ cl2 cl1 = first (arr (second swapEither) <<<) <$> initSchedule cl1 cl2
    swapEither :: Either a b -> Either b a -- TODO Why is stuff like this not in base? Maybe send pull request...
    swapEither (Left  a) = Right a
    swapEither (Right b) = Left  b

-- TODO I originally wanted to rescale a schedule and its clocks at the same time.
-- That's rescaleSequentialClock.
-- | If a schedule works for two clocks, a rescaling of the clocks
--   also applies to the schedule.
rescaledSchedule
  :: Monad m
  => Schedule m cl1 cl2
  -> Schedule m (RescaledClock cl1 time) (RescaledClock cl2 time)
rescaledSchedule schedule = Schedule $ initSchedule'
  where
    initSchedule' cl1 cl2 = initSchedule (rescaledScheduleS schedule) (rescaledClockToS cl1) (rescaledClockToS cl2)

-- | As 'rescaledSchedule', with a stateful rescaling
rescaledScheduleS
  :: Monad m
  => Schedule m cl1 cl2
  -> Schedule m (RescaledClockS m cl1 time tag1) (RescaledClockS m cl2 time tag2)
rescaledScheduleS Schedule {..} = Schedule initSchedule'
  where
    initSchedule' (RescaledClockS cl1 rescaleS1) (RescaledClockS cl2 rescaleS2) = do
      (runningSchedule, initTime ) <- initSchedule cl1 cl2
      (rescaling1     , initTime') <- rescaleS1 initTime
      (rescaling2     , _        ) <- rescaleS2 initTime
      let runningSchedule'
            = runningSchedule >>> proc (time, tag12) -> case tag12 of
                Left  tag1 -> do
                  (time', tag1') <- rescaling1 -< (time, tag1)
                  returnA -< (time', Left  tag1')
                Right tag2 -> do
                  (time', tag2') <- rescaling2 -< (time, tag2)
                  returnA -< (time', Right tag2')
      return (runningSchedule', initTime')



-- TODO What's the most general way we can lift a schedule this way?
-- | Lifts a schedule into the 'ReaderT' transformer,
--   supplying the same environment to its scheduled clocks.
readerSchedule
  :: ( Monad m
     , Clock (ReaderT r m) cl1, Clock (ReaderT r m) cl2
     , Time cl1 ~ Time cl2
     )
  => Schedule m
       (HoistClock (ReaderT r m) m cl1) (HoistClock (ReaderT r m) m cl2)
  -> Schedule (ReaderT r m) cl1 cl2
readerSchedule Schedule {..}
  = Schedule $ \cl1 cl2 -> ReaderT $ \r -> first liftMSFTrans
  <$> initSchedule
        (HoistClock cl1 $ flip runReaderT r)
        (HoistClock cl2 $ flip runReaderT r)


-- * Composite clocks

-- ** Sequentially composed clocks

-- | Two clocks can be combined with a schedule as a clock
--   for an asynchronous sequential composition of signal networks.
data SequentialClock m cl1 cl2
  = Time cl1 ~ Time cl2
  => SequentialClock
    { sequentialCl1      :: cl1
    , sequentialCl2      :: cl2
    , sequentialSchedule :: Schedule m cl1 cl2
    }

-- | Abbrevation synonym.
type SeqClock m cl1 cl2 = SequentialClock m cl1 cl2

instance (Monad m, Clock m cl1, Clock m cl2)
      => Clock m (SequentialClock m cl1 cl2) where
  type Time (SequentialClock m cl1 cl2) = Time cl1
  type Tag  (SequentialClock m cl1 cl2) = Either (Tag cl1) (Tag cl2)
  initClock SequentialClock {..}
    = initSchedule sequentialSchedule sequentialCl1 sequentialCl2

-- | Hoist a sequential clock, preserving its decomposition.
--   Hoists the individual clocks and the schedule.
hoistedSeqClock
  :: (Monad m, Monad m')
  => (forall a . m a -> m' a)
  -> SequentialClock m cl1 cl2
  -> SequentialClock m' (HoistClock m m' cl1)  (HoistClock m m' cl2)
hoistedSeqClock morph (SequentialClock {..}) = SequentialClock
  { sequentialCl1      = HoistClock sequentialCl1 morph
  , sequentialCl2      = HoistClock sequentialCl2 morph
  , sequentialSchedule = hoistClockSchedule morph sequentialSchedule
  }

-- ** Parallelly composed clocks

-- | Two clocks can be combined with a schedule as a clock
--   for an asynchronous parallel composition of signal networks.
data ParallelClock m cl1 cl2
  = Time cl1 ~ Time cl2
  => ParallelClock
    { parallelCl1      :: cl1
    , parallelCl2      :: cl2
    , parallelSchedule :: Schedule m cl1 cl2
    }

-- | Abbrevation synonym.
type ParClock m cl1 cl2 = ParallelClock m cl1 cl2

instance (Monad m, Clock m cl1, Clock m cl2)
      => Clock m (ParallelClock m cl1 cl2) where
  type Time (ParallelClock m cl1 cl2) = Time cl1
  type Tag  (ParallelClock m cl1 cl2) = Either (Tag cl1) (Tag cl2)
  initClock ParallelClock {..}
    = initSchedule parallelSchedule parallelCl1 parallelCl2

-- | Hoist a parallel clock, preserving its decomposition.
--   Hoists the individual clocks and the schedule.
hoistedParClock
  :: (Monad m, Monad m')
  => (forall a . m a -> m' a)
  -> ParallelClock m cl1 cl2
  -> ParallelClock m' (HoistClock m m' cl1)  (HoistClock m m' cl2)
hoistedParClock morph (ParallelClock {..}) = ParallelClock
  { parallelCl1      = HoistClock parallelCl1 morph
  , parallelCl2      = HoistClock parallelCl2 morph
  , parallelSchedule = hoistClockSchedule morph parallelSchedule
  }


-- * Navigating the clock tree

-- | The clock that represents the rate at which data enters the system.
type family In cl where
  In (SequentialClock m cl1 cl2) = In cl1
  In (ParallelClock   m cl1 cl2) = ParallelClock m (In cl1) (In cl2)
  In cl                          = cl

-- | The clock that represents the rate at which data leaves the system.
type family Out cl where
  Out (SequentialClock m cl1 cl2) = Out cl2
  Out (ParallelClock   m cl1 cl2) = ParallelClock m (Out cl1) (Out cl2)
  Out cl                          = cl


-- | A tree representing possible last times to which
--   the constituents of a clock may have ticked.
data LastTime cl where
  SequentialLastTime
    :: LastTime cl1 -> LastTime cl2
    -> LastTime (SequentialClock m cl1 cl2)
  ParallelLastTime
    :: LastTime cl1 -> LastTime cl2
    -> LastTime (ParallelClock   m cl1 cl2)
  LeafLastTime :: Time cl -> LastTime cl


-- | An inclusion of a clock into a tree of parallel compositions of clocks.
data ParClockInclusion clS cl where
  ParClockInL
    :: ParClockInclusion (ParallelClock m clL clR) cl
    -> ParClockInclusion                  clL      cl
  ParClockInR
    :: ParClockInclusion (ParallelClock m clL clR) cl
    -> ParClockInclusion                      clR  cl
  ParClockRefl :: ParClockInclusion cl cl

-- | Generates a tag for the composite clock from a tag of a leaf clock,
--   given a parallel clock inclusion.
parClockTagInclusion :: ParClockInclusion clS cl -> Tag clS -> Tag cl
parClockTagInclusion (ParClockInL parClockInL) tag = parClockTagInclusion parClockInL $ Left  tag
parClockTagInclusion (ParClockInR parClockInR) tag = parClockTagInclusion parClockInR $ Right tag
parClockTagInclusion ParClockRefl              tag = tag

{-
-- * Hoist composite clocks along monad morphisms

type family HoistedClock m m' cl where
  HoistedClock m m' (SequentialClock m cl1 cl2) = SequentialClock m' (HoistedClock m m' cl1)  (HoistedClock m m' cl2)
  HoistedClock m m' (ParallelClock   m cl1 cl2) = ParallelClock   m' (HoistedClock m m' cl1)  (HoistedClock m m' cl2)
  HoistedClock m m' cl = HoistClock m m' cl

hoistedClock :: (forall a . m a -> m' a) -> cl -> HoistedClock m m' cl
hoistedClock morph (SequentialClock {..}) = SequentialClock
  { sequentialCl1      = hoistedClock  morph sequentialCl1
  , sequentialCl2      = hoistedClock  morph sequentialCl1
  , sequentialSchedule = hoistSchedule morph sequentialSchedule
  }
-}
