{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
'Clock's are the central new notion in Rhine.
There are clock types (instances of the 'Clock' type class)
and their values.

This module provides the 'Clock' type class, several utilities,
and certain general constructions of 'Clock's,
such as clocks lifted along monad morphisms or time rescalings.
-}
module FRP.Rhine.Clock where


-- transformers
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)

-- automaton
import Data.Automaton (Automaton, hoistS)

-- time-domain
import Data.TimeDomain
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Time (getCurrentTime)
import Control.Concurrent (threadDelay)
import Control.Monad.Morph (MFunctor (..), MMonad (..))

-- * The 'Clock' type class

{- |
A clock creates a stream of time stamps and additional information,
possibly together with side effects in a monad 'm'
that cause the environment to wait until the specified time is reached.
-}
type RunningClock m time tag = Automaton m () (time, tag)

{- |
When initialising a clock, the initial time is measured
(typically by means of a side effect),
and a running clock is returned.
-}
type RunningClockInit m time tag = m (RunningClock m time tag, time)

{- |
Since we want to leverage Haskell's type system to annotate signal networks by their clocks,
each clock must be an own type, 'cl'.
Different values of the same clock type should tick at the same speed,
and only differ in implementation details.
Often, clocks are singletons.
-}
class (TimeDomain (Time cl), MonadTime m (Time cl)) => Clock m cl where
  -- | The time domain, i.e. type of the time stamps the clock creates.
  type Time cl

  -- | Additional information that the clock may output at each tick,
  --   e.g. if a realtime promise was met, if an event occurred,
  --   if one of its subclocks (if any) ticked.
  type Tag cl

  runClock :: cl -> Automaton m (Time cl) (Diff (Time cl), Tag cl)

-- * Auxiliary definitions and utilities

-- | An annotated, rich time stamp.
data TimeInfo cl = TimeInfo
  { sinceLast :: Diff (Time cl)
  -- ^ Time passed since the last tick
  , sinceInit :: Diff (Time cl)
  -- ^ Time passed since the initialisation of the clock
  , absolute :: Time cl
  -- ^ The absolute time of the current tick
  , tag :: Tag cl
  -- ^ The tag annotation of the current tick
  }

-- | A utility that changes the tag of a 'TimeInfo'.
retag ::
  (Time cl1 ~ Time cl2) =>
  (Tag cl1 -> Tag cl2) ->
  TimeInfo cl1 ->
  TimeInfo cl2
retag f TimeInfo {..} = TimeInfo {tag = f tag, ..}

-- * Certain universal building blocks to produce new clocks from given ones

-- | Applying a monad morphism yields a new clock.
data HoistClock m1 m2 cl = HoistClock
  { unhoistedClock :: cl
  , monadMorphism :: forall a. m1 a -> m2 a
  }

instance
  (Monad m1, Monad m2, Clock m1 cl, MonadTime m2 (Time cl)) =>
  Clock m2 (HoistClock m1 m2 cl)
  where
  type Time (HoistClock m1 m2 cl) = Time cl
  type Tag (HoistClock m1 m2 cl) = Tag cl
  runClock HoistClock {..} = hoistS monadMorphism $ runClock unhoistedClock
  {-# INLINE runClock #-}

-- | Lift a clock type into a monad transformer.
type LiftClock m t = HoistClock m (t m)

-- | Lift a clock value into a monad transformer.
liftClock :: (Monad m, MonadTrans t) => cl -> LiftClock m t cl
liftClock unhoistedClock =
  HoistClock
    { monadMorphism = lift
    , ..
    }

-- | Lift a clock type into 'MonadIO'.
type IOClock m cl = HoistClock IO m cl

-- | Lift a clock value into 'MonadIO'.
ioClock :: (MonadIO m) => cl -> IOClock m cl
ioClock unhoistedClock =
  HoistClock
    { monadMorphism = liftIO
    , ..
    }

-- *

class (TimeDomain td, MonadChangeset td (Diff td) m) => MonadTime m td where
  getTime :: m td
  getTime = current
  wait :: Diff td -> m ()
  wait = change

newtype UTCT m a = UTCT {getUTCT :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadPlus, Alternative)

instance MonadTrans UTCT where
  lift = UTCT

instance MFunctor UTCT where
  hoist f = UTCT . f . getUTCT

instance MMonad UTCT where
  embed f (UTCT ma)= f ma

instance MonadIO m => MonadChangeset UTCTime Double (UTCT m) where
  changeset f = UTCT $ liftIO $ do
    now <- getCurrentTime
    let (a, waitDiff) = f now
    threadDelay $ floor $ waitDiff * 1_000_000
    pure a

instance MonadIO m => MonadTime (UTCT m) UTCTime
