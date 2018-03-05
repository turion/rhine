{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
module FRP.Rhine.SyncSF.Core
  ( module FRP.Rhine.SyncSF.Core
  , module FRP.Rhine.Clock
  , module FRP.Rhine.TimeDomain
  , module Control.Arrow
  , module Data.MonadicStreamFunction
  )
  where

-- base
import Control.Arrow

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, mapReaderT, withReaderT)

-- dunai
import Data.MonadicStreamFunction (MSF, arrM, arrM_, liftMSFPurer, liftMSFTrans)

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
type SyncSignal m cl a = forall arbitrary . SyncSF m cl arbitrary a

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

-- | Hoist a 'SyncSF' along a monad morphism.
hoistSyncSF
  :: (Monad m1, Monad m2)
  => (forall c. m1 c -> m2 c)
  -> SyncSF m1 cl a b
  -> SyncSF m2 cl a b
hoistSyncSF hoist = liftMSFPurer $ mapReaderT hoist

-- | Hoist a 'SyncSF' and its clock along a monad morphism.
hoistSyncSFAndClock
  :: (Monad m1, Monad m2)
  => (forall c. m1 c -> m2 c)
  -> SyncSF m1 cl a b
  -> SyncSF m2 (HoistClock m1 m2 cl) a b
hoistSyncSFAndClock hoist
  = liftMSFPurer $ withReaderT (retag id) . mapReaderT hoist

-- | Lift a 'SyncSF' into a monad transformer.
liftSyncSF
  :: (Monad m, MonadTrans t, Monad (t m))
  => SyncSF    m  cl a b
  -> SyncSF (t m) cl a b
liftSyncSF = hoistSyncSF lift

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

{- | Call a 'SyncSF' every time the input is 'Just a'.

Caution: This will not change the time differences since the last tick.
For example,
while @integrate 1@ is approximately the same as @timeInfoOf sinceStart@,
@mapMaybe $ integrate 1@ is very different from
@mapMaybe $ timeInfoOf sinceStart@.
The former only integrates when the input is @Just 1@,
whereas the latter always returns the correct time since start of the program.
-}
mapMaybe
  :: Monad m
  => SyncSF m cl        a         b
  -> SyncSF m cl (Maybe a) (Maybe b)
mapMaybe behaviour = proc ma -> case ma of
  Nothing -> returnA                -< Nothing
  Just a  -> arr Just <<< behaviour -< a
-- TODO Consider integrating up the time deltas
