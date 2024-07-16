{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
The 'MonadSchedule' class from the @monad-schedule@ package is the compatibility mechanism between two different clocks.
It implements a concurrency abstraction that allows the clocks to run at the same time, independently.
Several such clocks running together form composite clocks, such as 'ParallelClock' and 'SequentialClock'.
This module defines these composite clocks,
and utilities to work with them.
-}
module FRP.Rhine.Schedule where

-- base
import Control.Arrow
import Data.List.NonEmpty as N

-- transformers
import Control.Monad.Trans.Reader

-- monad-schedule
import Control.Monad.Schedule.Class

-- automaton
import Data.Automaton
import Data.Automaton.Final (getFinal, toFinal)
import Data.Stream
import Data.Stream.Final qualified as StreamFinal
import Data.Stream.Optimized (OptimizedStreamT (..), toStreamT, concatS)
import Data.Stream.Result

-- rhine

import Control.Monad.State (runState)
import Control.Monad.State.Strict qualified as StateT
import Data.Foldable1 (Foldable1 (foldrMap1))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.Maybe (catMaybes, mapMaybe)
import Data.SOP (HAp, HCollapse (hcollapse), HPure (..), HSequence (htraverse'), I (..), K (K), NP (..), NS (..), Prod, SList (..), SListI, apInjs_NP, hliftA, hliftA2, hzipWith, sList, unI, type (-.->) (..))
import Data.SOP.Classes (HAp (..))
import Data.SOP.NP (liftA2_NP, liftA_NP)
import FRP.Rhine.Clock

-- * Scheduling

newtype Step m b state = Step {getStep :: ResultStateT state m b}

newtype RunningResult b state = RunningResult {getRunningResult :: Result state b}
newtype RunningResultT m b state = RunningResultT {getRunningResultT :: m (RunningResult b state)}

{-
-- n-ary nonempty product
data NNEP f ss where
  ZNE :: f s -> NP (Compose Maybe f) ss -> NNEP f (s ': ss)
  SNE :: NNEP f (s2 ': ss) -> NNEP f (s1 ': s2 ': ss)

type instance Prod NNEP = NP

instance HAp NNEP where
  hap (s :* ss) (ZNE fs nnep) = ZNE (apFn s fs) $ hzipWith (\fn -> Compose . fmap (apFn fn) . getCompose) ss nnep
  hap (_ :* ss) (SNE nnep) = SNE $ hap ss nnep
  hap Nil x = case x of {}

nnepToNP :: NNEP f states -> NP (Compose Maybe f) states
nnepToNP (ZNE fs np) = Compose (Just fs) :* np
nnepToNP (SNE nnep) = Compose Nothing :* nnepToNP nnep

consNNEP :: f x -> NNEP f xs -> NNEP f (x ': xs)
consNNEP fx nnep = ZNE fx $ nnepToNP nnep

productToNNEP :: NP f (x ': xs) -> NNEP f (x ': xs)
productToNNEP (x :* xs) = ZNE x $ hliftA (Compose . Just) xs

nnepToNonEmpty :: NNEP f (state ': states) -> NonEmpty (NS f (state ': states))
nnepToNonEmpty (ZNE state states) = Z state :| (S <$> mapMaybe (htraverse' getCompose) (apInjs_NP states))
nnepToNonEmpty (SNE states) = S <$> nnepToNonEmpty states
-}

apInjsNPNonEmpty :: (SListI xs) => NP f (x ': xs) -> NonEmpty (NS f (x ': xs))
apInjsNPNonEmpty (fx :* fxs) = Z fx :| (S <$> apInjs_NP fxs)

data Streams m b = forall state (states :: [Type]).
  (SListI states) =>
  Streams
  { states :: NP I (state ': states)
  , steps :: NP (Step m b) (state ': states)
  }

buildStreams :: StreamT m b -> Streams m b
buildStreams StreamT {state, step} =
  Streams
    { states = I state :* Nil
    , steps = Step (ResultStateT step) :* Nil
    }

consStreams :: StreamT m b -> Streams m b -> Streams m b
consStreams StreamT {state, step} Streams {states, steps} =
  Streams
    { states = I state :* states
    , steps = Step (ResultStateT step) :* steps
    }

scheduleStreams :: (MonadSchedule m, Functor m, Applicative m) => Streams m b -> StreamT m (NonEmpty b)
scheduleStreams Streams {states, steps} =
  StreamT
    { state = (apInjsNPNonEmpty states, [])
    , step = \(restingStates, runningStates) ->
        fmap (htraverse' getRunningResultT . hzipWith (\step -> RunningResultT . kick step . unI) steps) restingStates
          & flip appendList runningStates
          & schedule
          & fmap
            ( \(finished, running) ->
                let finishedStates = fmap (hliftA (I . resultState . getRunningResult)) finished
                    outputs =
                      finished
                        <&> (hliftA (getRunningResult >>> output >>> K) >>> hcollapse)
                 in Result (finishedStates, running) outputs
            )
    }
  where
    kick :: (Functor m) => Step m b state -> state -> m (RunningResult b state)
    kick Step {getStep} state = RunningResult <$> getResultStateT getStep state

scheduleStreams' :: (MonadSchedule m, Applicative m) => NonEmpty (StreamT m b) -> StreamT m (NonEmpty b)
scheduleStreams' ne = scheduleStreams $ foldrMap1 buildStreams consStreams ne

{- | Run several automata concurrently.

Whenever one automaton outputs a value,
it is returned together with all other values that happen to be output at the same time.
-}
scheduleList :: (Monad m, MonadSchedule m) => NonEmpty (Automaton m a b) -> Automaton m a (NonEmpty b)
scheduleList automatons0 =
  Automaton $
    Stateful $
      scheduleStreams' $
        toStreamT . getAutomaton <$> automatons0

{- | Run two automata concurrently.

Whenever one automaton returns a value, it is returned.
-}
schedulePair :: (Monad m, MonadSchedule m) => Automaton m a b -> Automaton m a b -> Automaton m a b
schedulePair automatonL automatonR = Automaton $ Data.Stream.Optimized.concatS $ fmap toList $ Stateful $ scheduleStreams' $ fmap (toStreamT . getAutomaton) $ automatonL :| [automatonR]

-- | Run two running clocks concurrently.
runningSchedule ::
  ( Monad m
  , MonadSchedule m
  , Clock m cl1
  , Clock m cl2
  , Time cl1 ~ Time cl2
  ) =>
  cl1 ->
  cl2 ->
  RunningClock m (Time cl1) (Tag cl1) ->
  RunningClock m (Time cl2) (Tag cl2) ->
  RunningClock m (Time cl1) (Either (Tag cl1) (Tag cl2))
runningSchedule _ _ rc1 rc2 = schedulePair (rc1 >>> arr (second Left)) (rc2 >>> arr (second Right))

{- | A schedule implements a combination of two clocks.
   It outputs a time stamp and an 'Either' value,
   which specifies which of the two subclocks has ticked.
-}
initSchedule ::
  ( Time cl1 ~ Time cl2
  , Monad m
  , MonadSchedule m
  , Clock m cl1
  , Clock m cl2
  ) =>
  cl1 ->
  cl2 ->
  RunningClockInit m (Time cl1) (Either (Tag cl1) (Tag cl2))
initSchedule cl1 cl2 = do
  (runningClock1, initTime) <- initClock cl1
  (runningClock2, _) <- initClock cl2
  return
    ( runningSchedule cl1 cl2 runningClock1 runningClock2
    , initTime
    )

-- * Composite clocks

-- ** Sequentially combined clocks

{- | Two clocks can be combined with a schedule as a clock
   for an asynchronous sequential composition of signal networks.
-}
data SequentialClock cl1 cl2 = (Time cl1 ~ Time cl2) =>
  SequentialClock
  { sequentialCl1 :: cl1
  , sequentialCl2 :: cl2
  }

-- | Abbrevation synonym.
type SeqClock cl1 cl2 = SequentialClock cl1 cl2

instance
  (Monad m, MonadSchedule m, Clock m cl1, Clock m cl2) =>
  Clock m (SequentialClock cl1 cl2)
  where
  type Time (SequentialClock cl1 cl2) = Time cl1
  type Tag (SequentialClock cl1 cl2) = Either (Tag cl1) (Tag cl2)
  initClock SequentialClock {..} =
    initSchedule sequentialCl1 sequentialCl2

-- ** Parallelly combined clocks

{- | Two clocks can be combined with a schedule as a clock
   for an asynchronous parallel composition of signal networks.
-}
data ParallelClock cl1 cl2 = (Time cl1 ~ Time cl2) =>
  ParallelClock
  { parallelCl1 :: cl1
  , parallelCl2 :: cl2
  }

-- | Abbrevation synonym.
type ParClock cl1 cl2 = ParallelClock cl1 cl2

instance
  (Monad m, MonadSchedule m, Clock m cl1, Clock m cl2) =>
  Clock m (ParallelClock cl1 cl2)
  where
  type Time (ParallelClock cl1 cl2) = Time cl1
  type Tag (ParallelClock cl1 cl2) = Either (Tag cl1) (Tag cl2)
  initClock ParallelClock {..} =
    initSchedule parallelCl1 parallelCl2

-- * Navigating the clock tree

-- | The clock that represents the rate at which data enters the system.
type family In cl where
  In (SequentialClock cl1 cl2) = In cl1
  In (ParallelClock cl1 cl2) = ParallelClock (In cl1) (In cl2)
  In cl = cl

-- | The clock that represents the rate at which data leaves the system.
type family Out cl where
  Out (SequentialClock cl1 cl2) = Out cl2
  Out (ParallelClock cl1 cl2) = ParallelClock (Out cl1) (Out cl2)
  Out cl = cl

{- | A tree representing possible last times to which
   the constituents of a clock may have ticked.
-}
data LastTime cl where
  SequentialLastTime ::
    LastTime cl1 ->
    LastTime cl2 ->
    LastTime (SequentialClock cl1 cl2)
  ParallelLastTime ::
    LastTime cl1 ->
    LastTime cl2 ->
    LastTime (ParallelClock cl1 cl2)
  LeafLastTime :: Time cl -> LastTime cl

-- | An inclusion of a clock into a tree of parallel compositions of clocks.
data ParClockInclusion clS cl where
  ParClockInL ::
    ParClockInclusion (ParallelClock clL clR) cl ->
    ParClockInclusion clL cl
  ParClockInR ::
    ParClockInclusion (ParallelClock clL clR) cl ->
    ParClockInclusion clR cl
  ParClockRefl :: ParClockInclusion cl cl

{- | Generates a tag for the composite clock from a tag of a leaf clock,
   given a parallel clock inclusion.
-}
parClockTagInclusion :: ParClockInclusion clS cl -> Tag clS -> Tag cl
parClockTagInclusion (ParClockInL parClockInL) tag = parClockTagInclusion parClockInL $ Left tag
parClockTagInclusion (ParClockInR parClockInR) tag = parClockTagInclusion parClockInR $ Right tag
parClockTagInclusion ParClockRefl tag = tag
