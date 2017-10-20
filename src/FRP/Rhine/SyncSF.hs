{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module FRP.Rhine.SyncSF where


-- base
import Control.Arrow
import Control.Category (Category)
import qualified Control.Category (id)

-- transformers
import Control.Monad.Trans.Reader
  ( ReaderT, ask, asks, mapReaderT, withReaderT)

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
--   that is, reading the current 'TimeInfo' of the clock 'cl'.
type SyncSF m cl a b = MSF (ReaderT (TimeInfo cl) m) a b

-- | A (side-effectful) behaviour is a time-aware stream
--   that doesn't depend on a particular clock.
--   'td' denotes the time domain.
type Behaviour m td a = forall cl. td ~ TimeDomainOf cl => SyncSF m cl () a

-- | Compatibility to U.S. american spelling.
type Behavior  m td a = Behaviour m td a


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
     , Groundfield v ~ Diff (TimeDomainOf cl))
  => v -> SyncSF m cl v v
integralFrom v0 = proc v -> do
  _sinceTick <- timeInfoOf sinceTick -< ()
  sumFrom v0                         -< _sinceTick *^ v

-- | Euler integration, with zero initial offset.
integral
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff (TimeDomainOf cl))
  => SyncSF m cl v v
integral = integralFrom zeroVector


-- | The output of @derivativeFrom v0@ is the numerical derivative of the input,
--   with a Newton difference quotient.
--   The input is initialised with @v0@.
derivativeFrom
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff (TimeDomainOf cl))
  => v -> SyncSF m cl v v
derivativeFrom v0 = proc v -> do
  vLast         <- delay v0 -< v
  TimeInfo {..} <- timeInfo -< ()
  returnA                   -< (v ^-^ vLast) ^/ sinceTick

-- | Numerical derivative with input initialised to zero.
derivative
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff (TimeDomainOf cl))
  => SyncSF m cl v v
derivative = derivativeFrom zeroVector


-- | An average, or low pass. It will average out, or filter,
--   all features below a given time scale.
averageFrom
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff (TimeDomainOf cl))
  => v -- ^ The initial position
  -> Diff (TimeDomainOf cl) -- ^ The time scale on which the signal is averaged
  -> SyncSF m cl v v
averageFrom v0 t = feedback v0 $ proc (v, vAvg) -> do
  TimeInfo {..} <- timeInfo -< ()
  let vAvg' = (v ^* sinceTick ^+^ vAvg ^* t) ^/ (sinceTick + t)
  returnA                   -< (vAvg', vAvg')


-- | An average, or low pass, initialised to zero.
average
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff (TimeDomainOf cl))
  => Diff (TimeDomainOf cl) -- ^ The time scale on which the signal is averaged
  -> SyncSF m cl v v
average = averageFrom zeroVector
