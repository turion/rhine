{- |
The core functionality of clocked signal functions,
supplying the type of clocked signal functions itself ('ClSF'),
behaviours (clock-independent/polymorphic signal functions),
and basic constructions of 'ClSF's that may use awareness of time as an effect.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.ClSF.Core
  ( module FRP.Rhine.ClSF.Core
  , module Control.Arrow
  , module X
  )
  where

-- base
import Control.Arrow

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, mapReaderT, withReaderT)

-- dunai
import Data.MonadicStreamFunction as X hiding ((>>>^), (^>>>))

-- rhine
import FRP.Rhine.Clock      as X


-- * Clocked signal functions and behaviours

-- | A (synchronous, clocked) monadic stream function
--   with the additional side effect of being time-aware,
--   that is, reading the current 'TimeInfo' of the clock @cl@.
type ClSF m cl a b = MSF (ReaderT (TimeInfo cl) m) a b

-- | A clocked signal is a 'ClSF' with no input required.
--   It produces its output on its own.
type ClSignal m cl a = forall arbitrary . ClSF m cl arbitrary a

-- | A (side-effectful) behaviour is a time-aware stream
--   that doesn't depend on a particular clock.
--   @time@ denotes the 'TimeDomain'.
type Behaviour m time a = forall cl. time ~ Time cl => ClSignal m cl a

-- | Compatibility to U.S. american spelling.
type Behavior  m time a = Behaviour m time a

-- | A (side-effectful) behaviour function is a time-aware synchronous stream
--   function that doesn't depend on a particular clock.
--   @time@ denotes the 'TimeDomain'.
type BehaviourF m time a b = forall cl. time ~ Time cl => ClSF m cl a b

-- | Compatibility to U.S. american spelling.
type BehaviorF  m time a b = BehaviourF m time a b

-- * Utilities to create 'ClSF's from simpler data

-- | Hoist a 'ClSF' along a monad morphism.
hoistClSF
  :: (Monad m1, Monad m2)
  => (forall c. m1 c -> m2 c)
  -> ClSF m1 cl a b
  -> ClSF m2 cl a b
hoistClSF hoist = morphS $ mapReaderT hoist

-- | Hoist a 'ClSF' and its clock along a monad morphism.
hoistClSFAndClock
  :: (Monad m1, Monad m2)
  => (forall c. m1 c -> m2 c)
  -> ClSF m1 cl a b
  -> ClSF m2 (HoistClock m1 m2 cl) a b
hoistClSFAndClock hoist
  = morphS $ withReaderT (retag id) . mapReaderT hoist

-- | Lift a 'ClSF' into a monad transformer.
liftClSF
  :: (Monad m, MonadTrans t, Monad (t m))
  => ClSF    m  cl a b
  -> ClSF (t m) cl a b
liftClSF = hoistClSF lift

-- | Lift a 'ClSF' and its clock into a monad transformer.
liftClSFAndClock
  :: (Monad m, MonadTrans t, Monad (t m))
  => ClSF    m                 cl  a b
  -> ClSF (t m) (LiftClock m t cl) a b
liftClSFAndClock = hoistClSFAndClock lift

-- | A monadic stream function without dependency on time
--   is a 'ClSF' for any clock.
timeless :: Monad m => MSF m a b -> ClSF m cl a b
timeless = liftTransS

-- | Utility to lift Kleisli arrows directly to 'ClSF's.
arrMCl :: Monad m => (a -> m b) -> ClSF m cl a b
arrMCl = timeless . arrM

-- | Version without input.
constMCl :: Monad m => m b -> ClSF m cl a b
constMCl = timeless . constM

{- | Call a 'ClSF' every time the input is 'Just a'.

Caution: This will not change the time differences since the last tick.
For example,
while @integrate 1@ is approximately the same as @timeInfoOf sinceInit@,
@mapMaybe $ integrate 1@ is very different from
@mapMaybe $ timeInfoOf sinceInit@.
The former only integrates when the input is @Just 1@,
whereas the latter always returns the correct time since initialisation.
-}
mapMaybe
  :: Monad m
  => ClSF m cl        a         b
  -> ClSF m cl (Maybe a) (Maybe b)
mapMaybe behaviour = proc ma -> case ma of
  Nothing -> returnA                -< Nothing
  Just a  -> arr Just <<< behaviour -< a
-- TODO Consider integrating up the time deltas
