{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

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
  = (TimeDomainOf cl1 ~ TimeDomainOf cl2)
  => Schedule
    { startSchedule
        :: cl1 -> cl2
        -> RunningClockStarter m (TimeDomainOf cl1) (Either (Tag cl1) (Tag cl2))
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
hoistSchedule hoist Schedule {..} = Schedule startSchedule'
  where
    startSchedule' cl1 cl2 = hoist
      $ first (hoistMSF hoist) <$> startSchedule cl1 cl2
    hoistMSF = liftMSFPurer
    -- TODO This should be a dunai issue

-- | Swaps the clocks for a given schedule.
flipSchedule
  :: Monad m
  => Schedule m cl1 cl2
  -> Schedule m cl2 cl1
flipSchedule Schedule {..} = Schedule startSchedule_
  where
    startSchedule_ cl2 cl1 = first (arr (second swapEither) <<<) <$> startSchedule cl1 cl2
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
  -> Schedule m (RescaledClock cl1 td) (RescaledClock cl2 td)
rescaledSchedule schedule = Schedule $ startSchedule'
  where
    startSchedule' cl1 cl2 = startSchedule (rescaledScheduleS schedule) (rescaledClockToS cl1) (rescaledClockToS cl2)

-- | As 'rescaledSchedule', with a stateful rescaling
rescaledScheduleS
  :: Monad m
  => Schedule m cl1 cl2
  -> Schedule m (RescaledClockS m cl1 td tag1) (RescaledClockS m cl2 td tag2)
rescaledScheduleS Schedule {..} = Schedule startSchedule'
  where
    startSchedule' (RescaledClockS cl1 rescaleS1) (RescaledClockS cl2 rescaleS2) = do
      (runningSchedule, initTime ) <- startSchedule cl1 cl2
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
     , TimeDomainOf cl1 ~ TimeDomainOf cl2
     )
  => Schedule m
       (HoistClock (ReaderT r m) m cl1) (HoistClock (ReaderT r m) m cl2)
  -> Schedule (ReaderT r m) cl1 cl2
readerSchedule Schedule {..}
  = Schedule $ \cl1 cl2 -> ReaderT $ \r -> first liftMSFTrans
  <$> startSchedule
        (HoistClock cl1 $ flip runReaderT r)
        (HoistClock cl2 $ flip runReaderT r)


-- * Composite clocks


-- | Two clocks can be combined with a schedule as a clock
--   for an asynchronous sequential composition of signal functions.
data SequentialClock m cl1 cl2
  = TimeDomainOf cl1 ~ TimeDomainOf cl2
  => SequentialClock
    { sequentialCl1      :: cl1
    , sequentialCl2      :: cl2
    , sequentialSchedule :: Schedule m cl1 cl2
    }


instance (Monad m, Clock m cl1, Clock m cl2)
      => Clock m (SequentialClock m cl1 cl2) where
  type TimeDomainOf (SequentialClock m cl1 cl2) = TimeDomainOf cl1
  type Tag          (SequentialClock m cl1 cl2) = Either (Tag cl1) (Tag cl2)
  startClock SequentialClock {..}
    = startSchedule sequentialSchedule sequentialCl1 sequentialCl2


-- | Two clocks can be combined with a schedule as a clock
--   for an asynchronous parallel composition of signal functions.
data ParallelClock m cl1 cl2
  = TimeDomainOf cl1 ~ TimeDomainOf cl2
  => ParallelClock
    { parallelCl1      :: cl1
    , parallelCl2      :: cl2
    , parallelSchedule :: Schedule m cl1 cl2
    }

instance (Monad m, Clock m cl1, Clock m cl2)
      => Clock m (ParallelClock m cl1 cl2) where
  type TimeDomainOf (ParallelClock m cl1 cl2) = TimeDomainOf cl1
  type Tag          (ParallelClock m cl1 cl2) = Either (Tag cl1) (Tag cl2)
  startClock ParallelClock {..}
    = startSchedule parallelSchedule parallelCl1 parallelCl2


-- * Navigating the clock tree

-- | The clock that represents the rate at which data enters the system.
type family Leftmost cl where
  Leftmost (SequentialClock m cl1 cl2) = Leftmost cl1
  Leftmost (ParallelClock   m cl1 cl2) = ParallelClock m (Leftmost cl1) (Leftmost cl2)
  Leftmost cl                          = cl

-- | The clock that represents the rate at which data leaves the system.
type family Rightmost cl where
  Rightmost (SequentialClock m cl1 cl2) = Rightmost cl2
  Rightmost (ParallelClock   m cl1 cl2) = ParallelClock m (Rightmost cl1) (Rightmost cl2)
  Rightmost cl                          = cl


-- | A tree representing possible last times to which
--   the constituents of a clock may have ticked.
data LastTime cl where
  SequentialLastTime
    :: LastTime cl1 -> LastTime cl2
    -> LastTime (SequentialClock m cl1 cl2)
  ParallelLastTime
    :: LastTime cl1 -> LastTime cl2
    -> LastTime (ParallelClock   m cl1 cl2)
  LeafLastTime :: TimeDomainOf cl -> LastTime cl


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
