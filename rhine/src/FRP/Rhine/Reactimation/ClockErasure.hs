{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Translate clocked signal processing components to stream functions without explicit clock types.

This module is not meant to be used externally,
and is thus not exported from 'FRP.Rhine'.
-}
module FRP.Rhine.Reactimation.ClockErasure where

-- automaton
import Data.Automaton.Trans.Reader
import Data.Stream.Result (Result (..))

-- rhine
import FRP.Rhine.ClSF hiding (runReaderS)
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Clock.Util
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.SN.Type (SN (..))

{- | Run a clocked signal function as an automaton,
   accepting the timestamps and tags as explicit inputs.
-}
eraseClockClSF ::
  (Monad m, Clock m cl) =>
  ClockProxy cl ->
  Time cl ->
  ClSF m cl a b ->
  Automaton m (Time cl, Tag cl, a) b
eraseClockClSF proxy initialTime clsf = proc (time, tag, a) -> do
  timeInfo <- genTimeInfo proxy initialTime -< (time, tag)
  runReaderS clsf -< (timeInfo, a)
{-# INLINE eraseClockClSF #-}

{- | Remove the signal network type abstraction and reveal the underlying automaton.

* To drive the network, the timestamps and tags of the clock are needed
* Since the input and output clocks are not always guaranteed to tick, the inputs and outputs are 'Maybe'.
-}
eraseClockSN ::
  -- | Initial time
  Time cl ->
  -- The original signal network
  SN m cl a b ->
  Automaton m (Time cl, Tag cl, Maybe a) (Maybe b)
eraseClockSN time = flip runReader time . getSN
{-# INLINE eraseClockSN #-}

{- | Translate a resampling buffer into an automaton.

   The input decides whether the buffer is to accept input or has to produce output.
   (In the latter case, only time information is provided.)
-}
eraseClockResBuf ::
  ( Monad m
  , Clock m cl1
  , Clock m cl2
  , Time cl1 ~ Time cl2
  ) =>
  ClockProxy cl1 ->
  ClockProxy cl2 ->
  Time cl1 ->
  ResBuf m cl1 cl2 a b ->
  Automaton m (Either (Time cl1, Tag cl1, a) (Time cl2, Tag cl2)) (Maybe b)
eraseClockResBuf proxy1 proxy2 initialTime ResamplingBuffer {buffer, put, get} = feedback buffer $ proc (input, resBuf) -> do
  case input of
    Left (time1, tag1, a) -> do
      timeInfo1 <- genTimeInfo proxy1 initialTime -< (time1, tag1)
      resBuf' <- arrM (uncurry $ uncurry put) -< ((timeInfo1, a), resBuf)
      returnA -< (Nothing, resBuf')
    Right (time2, tag2) -> do
      timeInfo2 <- genTimeInfo proxy2 initialTime -< (time2, tag2)
      Result resBuf' b <- arrM (uncurry get) -< (timeInfo2, resBuf)
      returnA -< (Just b, resBuf')
{-# INLINE eraseClockResBuf #-}
