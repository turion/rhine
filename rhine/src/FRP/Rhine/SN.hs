{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Asynchronous signal networks are combinations of clocked signal functions ('ClSF's)
and matching 'ResamplingBuffer's,
all satisfying the appropriate clock type constraints.

This module defines the 'SN' type,
combinators are found in a submodule.
-}
module FRP.Rhine.SN (
  module FRP.Rhine.SN,
  module FRP.Rhine.SN.Type,
) where

-- base
import Control.Monad (join)

-- transformers
import Control.Monad.Trans.Reader (reader)

-- automata
import Data.Stream.Result (Result (..))

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Clock.Util (genTimeInfo)
import FRP.Rhine.Reactimation.ClockErasure
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.SN.Type
import FRP.Rhine.Schedule

{- | A synchronous automaton is the basic building block.
  For such an 'SN', data enters and leaves the system at the same rate as it is processed.
-}
synchronous ::
  forall cl m a b.
  (cl ~ In cl, cl ~ Out cl, Monad m, Clock m cl, GetClockProxy cl) =>
  ClSF m cl a b ->
  SN m cl a b
synchronous clsf = SN $ reader $ \initialTime -> proc (time, tag, Just a) -> do
  b <- eraseClockClSF (getClockProxy @cl) initialTime clsf -< (time, tag, a)
  returnA -< Just b
{-# INLINE synchronous #-}

-- | Two 'SN's may be sequentially composed if there is a matching 'ResamplingBuffer' between them.
sequential ::
  ( Clock m clab
  , Clock m clcd
  , Clock m (Out clab)
  , Clock m (Out clcd)
  , Clock m (In clab)
  , Clock m (In clcd)
  , GetClockProxy clab
  , GetClockProxy clcd
  , Time clab ~ Time clcd
  , Time clab ~ Time (Out clab)
  , Time clcd ~ Time (In clcd)
  , Monad m
  ) =>
  SN m clab a b ->
  ResamplingBuffer m (Out clab) (In clcd) b c ->
  SN m clcd c d ->
  SN m (SequentialClock clab clcd) a d
-- A sequentially composed signal network may either be triggered in its first component,
-- or its second component. In either case,
-- the resampling buffer (which connects the two components) may be triggered,
-- but only if the outgoing clock of the first component ticks,
-- or the incoming clock of the second component ticks.
sequential sn1 resBuf sn2 = SN $ reader $ \initialTime ->
  let
    proxy1 = toClockProxy sn1
    proxy2 = toClockProxy sn2
   in
    proc (time, tag, maybeA) -> do
      resBufIn <- case tag of
        Left tagL -> do
          maybeB <- eraseClockSN initialTime sn1 -< (time, tagL, maybeA)
          returnA -< Left <$> ((time,,) <$> outTag proxy1 tagL <*> maybeB)
        Right tagR -> do
          returnA -< Right . (time,) <$> inTag proxy2 tagR
      maybeC <- mapMaybeS $ eraseClockResBuf (outProxy proxy1) (inProxy proxy2) initialTime resBuf -< resBufIn
      case tag of
        Left _ -> do
          returnA -< Nothing
        Right tagR -> do
          eraseClockSN initialTime sn2 -< (time, tagR, join maybeC)
{-# INLINE sequential #-}

-- | Two 'SN's with the same input and output data may be parallely composed.
parallel snL snR = SN $ reader $ \initialTime -> proc (time, tag, maybeA) -> do
  case tag of
    Left tagL -> eraseClockSN initialTime snL -< (time, tagL, maybeA)
    Right tagR -> eraseClockSN initialTime snR -< (time, tagR, maybeA)
{-# INLINE parallel #-}

-- | A 'ClSF' can always be postcomposed onto an 'SN' if the clocks match on the output.
postcompose sn clsf = SN $ reader $ \initialTime ->
  let
    proxy = toClockProxy sn
   in
    proc input@(time, tag, _) -> do
      bMaybe <- eraseClockSN initialTime sn -< input
      mapMaybeS $ eraseClockClSF (outProxy proxy) initialTime clsf -< (time,,) <$> outTag proxy tag <*> bMaybe
{-# INLINE postcompose #-}

-- | A 'ClSF' can always be precomposed onto an 'SN' if the clocks match on the input.
precompose clsf sn = SN $ reader $ \initialTime ->
  let
    proxy = toClockProxy sn
   in
    proc (time, tag, aMaybe) -> do
      bMaybe <- mapMaybeS $ eraseClockClSF (inProxy proxy) initialTime clsf -< (time,,) <$> inTag proxy tag <*> aMaybe
      eraseClockSN initialTime sn -< (time, tag, bMaybe)
{-# INLINE precompose #-}

{- | Data can be looped back to the beginning of an 'SN',
  but it must be resampled since the 'Out' and 'In' clocks are generally different.
-}
feedbackSN ResamplingBuffer {buffer, put, get} sn = SN $ reader $ \initialTime ->
  let
    proxy = toClockProxy sn
   in
    feedback buffer $ proc ((time, tag, aMaybe), buf) -> do
      (cMaybe, buf') <- case inTag proxy tag of
        Nothing -> do
          returnA -< (Nothing, buf)
        Just tagIn -> do
          timeInfo <- genTimeInfo (inProxy proxy) initialTime -< (time, tagIn)
          Result buf' c <- arrM $ uncurry get -< (timeInfo, buf)
          returnA -< (Just c, buf')
      bdMaybe <- eraseClockSN initialTime sn -< (time, tag, (,) <$> aMaybe <*> cMaybe)
      case (,) <$> outTag proxy tag <*> bdMaybe of
        Nothing -> do
          returnA -< (Nothing, buf')
        Just (tagOut, (b, d)) -> do
          timeInfo <- genTimeInfo (outProxy proxy) initialTime -< (time, tagOut)
          buf'' <- arrM $ uncurry $ uncurry put -< ((timeInfo, d), buf')
          returnA -< (Just b, buf'')
{-# INLINE feedbackSN #-}

-- | Bypass the signal network by forwarding data in parallel through a 'ResamplingBuffer'.
firstResampling sn buf = SN $ reader $ \initialTime ->
  let
    proxy = toClockProxy sn
   in
    proc (time, tag, acMaybe) -> do
      bMaybe <- eraseClockSN initialTime sn -< (time, tag, fst <$> acMaybe)
      let
        resBufInput = case (inTag proxy tag, outTag proxy tag, snd <$> acMaybe) of
          (Just tagIn, _, Just c) -> Just $ Left (time, tagIn, c)
          (_, Just tagOut, _) -> Just $ Right (time, tagOut)
          _ -> Nothing
      dMaybe <- mapMaybeS $ eraseClockResBuf (inProxy proxy) (outProxy proxy) initialTime buf -< resBufInput
      returnA -< (,) <$> bMaybe <*> join dMaybe
{-# INLINE firstResampling #-}
