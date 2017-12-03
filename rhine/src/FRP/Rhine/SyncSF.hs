{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}

module FRP.Rhine.SyncSF where


-- base
import Control.Arrow
import Control.Category (Category)
import qualified Control.Category (id)

-- transformers
import Control.Monad.Trans.Reader
  (ReaderT, ask, asks, mapReaderT, withReaderT)

-- dunai
import Data.MonadicStreamFunction
  (MSF, liftMSFPurer, liftMSFTrans, arrM, arrM_, sumFrom, delay, feedback)
import Data.VectorSpace

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.TimeDomain


-- * Synchronous signal functions and behaviours

-- | A (synchronous) monadic stream function
--   with the additional side effect of being time-aware,
--   that is, reading the current 'TimeInfo' of the clock @cl@.
type SyncSF m cl a b = MSF (ReaderT (TimeInfo cl) m) a b

-- | A synchronous signal is a 'SyncSF' with no input required.
--   It produces its output on its own.
type SyncSignal m cl a = SyncSF m cl () a

-- | A (side-effectful) behaviour is a time-aware stream
--   that doesn't depend on a particular clock.
--   @td@ denotes the 'TimeDomain'.
type Behaviour m td a = forall cl. td ~ TimeDomainOf cl => SyncSignal m cl a

-- | Compatibility to U.S. american spelling.
type Behavior  m td a = Behaviour m td a

-- | A (side-effectful) behaviour function is a time-aware synchronous stream
--   function that doesn't depend on a particular clock.
--   @td@ denotes the 'TimeDomain'.
type BehaviourF m td a b = forall cl. td ~ TimeDomainOf cl => SyncSF m cl a b

-- | Compatibility to U.S. american spelling.
type BehaviorF  m td a b = BehaviourF m td a b


-- * Utilities to create 'SyncSF's from simpler data

-- TODO Test in which situations it makes sense not to change cl
-- | Hoist a 'SyncSF' along a monad morphism.
hoistSyncSF
  :: (Monad m1, Monad m2)
  => (forall c. m1 c -> m2 c)
  -> SyncSF m1 cl a b
  -> SyncSF m2 (HoistClock m1 m2 cl) a b
hoistSyncSF hoist = liftMSFPurer $ withReaderT (retag id) . mapReaderT hoist

-- | A monadic stream function without dependency on time
--   is a 'SyncSF' for any clock.
timeless :: Monad m => MSF m a b -> SyncSF m cl a b
timeless = liftMSFTrans

-- | Utility to lift Kleisli arrows directly to 'SyncSF's.
arrMSync :: Monad m => (a -> m b) -> SyncSF m cl a b
arrMSync = timeless . arrM

-- | Version without input.
arrMSync_ :: Monad m => m b -> SyncSF m cl a b
arrMSync_ = timeless . arrM_

-- | Read the environment variable, i.e. the 'TimeInfo'.
timeInfo :: Monad m => SyncSF m cl a (TimeInfo cl)
timeInfo = arrM_ ask

{- | Utility to apply functions to the current 'TimeInfo',
such as record selectors:
@
printAbsoluteTime :: SyncSF IO cl () ()
printAbsoluteTime = timeInfoOf absolute >>> arrMSync print
@
-}
timeInfoOf :: Monad m => (TimeInfo cl -> b) -> SyncSF m cl a b
timeInfoOf f = arrM_ $ asks f

-- * Useful aliases

-- TODO Is it cleverer to generalise to Arrow?
{- | Alias for 'Control.Category.>>>' (sequential composition)
with higher operator precedence, designed to work with the other operators, e.g.:

> syncsf1 >-> syncsf2 @@ clA **@ sched @** syncsf3 >-> syncsf4 @@ clB

The type signature specialises e.g. to

> (>->) :: Monad m => SyncSF m cl a b -> SyncSF m cl b c -> SyncSF m cl a c
-}
infixr 6 >->
(>->) :: Category cat
      => cat a b
      -> cat   b c
      -> cat a   c
(>->) = (>>>)

-- | Alias for 'Control.Category.<<<'.
infixl 6 <-<
(<-<) :: Category cat
      => cat   b c
      -> cat a b
      -> cat a   c
(<-<) = (<<<)

{- | Output a constant value.
Specialises e.g. to this type signature:

> arr_ :: Monad m => b -> SyncSF m cl a b
-}
arr_ :: Arrow a => b -> a c b
arr_ = arr . const


-- | The identity synchronous stream function.
syncId :: Monad m => SyncSF m cl a a
syncId = Control.Category.id


-- * Basic signal processing components

-- | The output of @integralFrom v0@ is the numerical Euler integral
--   of the input, with initial offset @v0@.
integralFrom
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => v -> BehaviorF m td v v
integralFrom v0 = proc v -> do
  _sinceTick <- timeInfoOf sinceTick -< ()
  sumFrom v0                         -< _sinceTick *^ v

-- | Euler integration, with zero initial offset.
integral
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => BehaviorF m td v v
integral = integralFrom zeroVector


-- | The output of @derivativeFrom v0@ is the numerical derivative of the input,
--   with a Newton difference quotient.
--   The input is initialised with @v0@.
derivativeFrom
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => v -> BehaviorF m td v v
derivativeFrom v0 = proc v -> do
  vLast         <- delay v0 -< v
  TimeInfo {..} <- timeInfo -< ()
  returnA                   -< (v ^-^ vLast) ^/ sinceTick

-- | Numerical derivative with input initialised to zero.
derivative
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => BehaviorF m td v v
derivative = derivativeFrom zeroVector

-- | A weighted moving average signal function.
--   The output is the average of the first input,
--   weighted by the second input
--   (which is assumed to be always between 0 and 1).
--   The weight is applied to the average of the last tick,
--   so a weight of 1 simply repeats the past value unchanged,
--   whereas a weight of 0 outputs the current value.
weightedAverageFrom
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => v -- ^ The initial position
  -> BehaviorF m td (v, Groundfield v) v
weightedAverageFrom v0 = feedback v0 $ proc ((v, weight), vAvg) -> do
  let
    vAvg' = weight *^ vAvg ^+^ (1 - weight) *^ v
  returnA -< (vAvg', vAvg')

-- | An exponential moving average, or low pass.
--   It will average out, or filter,
--   all features below a given time scale.
averageFrom
  :: ( Monad m, VectorSpace v
     , Floating (Groundfield v)
     , Groundfield v ~ Diff td)
  => v -- ^ The initial position
  -> Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviorF m td v v
averageFrom v0 t = proc v -> do
  TimeInfo {..} <- timeInfo -< ()
  let
    weight = exp $ - (sinceTick / t)
  weightedAverageFrom v0    -< (v, weight)


-- | An average, or low pass, initialised to zero.
average
  :: ( Monad m, VectorSpace v
     , Floating (Groundfield v)
     , Groundfield v ~ Diff td)
  => Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviourF m td v v
average = averageFrom zeroVector

-- | A linearised version of 'averageFrom'.
--   It is more efficient, but only accurate
--   if the supplied time scale is much bigger
--   than the average time difference between two ticks.
averageLinFrom
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => v -- ^ The initial position
  -> Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviourF m td v v
averageLinFrom v0 t = proc v -> do
  TimeInfo {..} <- timeInfo -< ()
  let
    weight = t / (sinceTick + t)
  weightedAverageFrom v0    -< (v, weight)

-- | Linearised version of 'average'.
averageLin
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviourF m td v v
averageLin = averageLinFrom zeroVector
