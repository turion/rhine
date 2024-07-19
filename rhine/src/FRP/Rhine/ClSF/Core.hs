{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
The core functionality of clocked signal functions,
supplying the type of clocked signal functions itself ('ClSF'),
behaviours (clock-independent/polymorphic signal functions),
and basic constructions of 'ClSF's that may use awareness of time as an effect.
-}
module FRP.Rhine.ClSF.Core (
  module FRP.Rhine.ClSF.Core,
  module Control.Arrow,
  module X,
)
where

-- base
import Control.Arrow

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, mapReaderT, withReaderT)

-- automaton
import Data.Automaton as X

-- rhine

import Data.Proxy (Proxy (..))
import Data.SOP (All, SListI)
import FRP.Rhine.Clock
import FRP.Rhine.SN.Tick

-- * Clocked signal functions and behaviours

type ClsSF m cls a b = Automaton (ReaderT (Tick cls) m) a b

{- | A (synchronous, clocked) automaton
   with the additional side effect of being time-aware,
   that is, reading the current 'TimeInfo' of the clock @cl@.
-}
type ClSF m cl a b = ClsSF m '[cl] a b

{- | A clocked signal is a 'ClSF' with no input required.
   It produces its output on its own.
-}
type ClSignal m cl a = forall arbitrary. ClSF m cl arbitrary a
type ClsSignal m cls a = forall arbitrary. ClsSF m cls arbitrary a

{- | A (side-effectful) behaviour is a time-aware stream
   that doesn't depend on a particular clock.
   @time@ denotes the 'TimeDomain'.
-}
type Behaviour m time a = forall cls. (All (HasTimeDomain time) cls) => ClsSignal m cls a

-- | Compatibility to U.S. american spelling.
type Behavior m time a = Behaviour m time a

{- | A (side-effectful) behaviour function is a time-aware synchronous stream
   function that doesn't depend on a particular clock.
   @time@ denotes the 'TimeDomain'.
-}
type BehaviourF m time a b = forall cls. (All (HasTimeDomain time) cls) => ClsSF m cls a b

-- | Compatibility to U.S. american spelling.
type BehaviorF m time a b = BehaviourF m time a b

-- * Utilities to create 'ClSF's from simpler data

-- | Hoist a 'ClSF' along a monad morphism.
hoistClSF ::
  (Monad m1, Monad m2) =>
  (forall c. m1 c -> m2 c) ->
  ClSF m1 cl a b ->
  ClSF m2 cl a b
hoistClSF hoist = hoistS $ mapReaderT hoist

-- | Hoist a 'ClSF' and its clock along a monad morphism.
hoistClSFAndClock ::
  (Monad m1, Monad m2) =>
  (forall c. m1 c -> m2 c) ->
  ClSF m1 cl a b ->
  ClSF m2 (HoistClock m1 m2 cl) a b
hoistClSFAndClock = hoistClsSFAndClocks

-- | Hoist a 'ClsSF' and its clocks along a monad morphism.
hoistClsSFAndClocks ::
  forall m1 m2 cls a b.
  (Monad m1, Monad m2, SListI cls) =>
  (forall c. m1 c -> m2 c) ->
  ClsSF m1 cls a b ->
  ClsSF m2 (Map (HoistClock m1 m2) cls) a b
hoistClsSFAndClocks hoist = hoistS $ withReaderT (retick $ retagHoistClock (Proxy @m1) (Proxy @m2)) . mapReaderT hoist
  where
    retagHoistClock :: Proxy m1 -> Proxy m2 -> TimeInfo (HoistClock m1 m2 cl) -> TimeInfo cl
    retagHoistClock _ _ = retag id

-- | Lift a 'ClSF' into a monad transformer.
liftClSF ::
  (Monad m, MonadTrans t, Monad (t m)) =>
  ClSF m cl a b ->
  ClSF (t m) cl a b
liftClSF = hoistClSF lift

-- | Lift a 'ClSF' and its clock into a monad transformer.
liftClSFAndClock ::
  (Monad m, MonadTrans t, Monad (t m)) =>
  ClSF m cl a b ->
  ClSF (t m) (LiftClock m t cl) a b
liftClSFAndClock = hoistClSFAndClock lift

{- | An automaton without dependency on time
   is a 'ClSF' for any clock.
-}
timeless :: (Monad m) => Automaton m a b -> ClSF m cl a b
timeless = liftS

-- | Utility to lift Kleisli arrows directly to 'ClSF's.
arrMCl :: (Monad m) => (a -> m b) -> ClSF m cl a b
arrMCl = timeless . arrM

-- | Version without input.
constMCl :: (Monad m) => m b -> ClSF m cl a b
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
mapMaybe ::
  (Monad m) =>
  ClSF m cl a b ->
  ClSF m cl (Maybe a) (Maybe b)
mapMaybe behaviour = proc ma -> case ma of
  Nothing -> returnA -< Nothing
  Just a -> arr Just <<< behaviour -< a

-- TODO Consider integrating up the time deltas
