{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Translate clocked signal processing components to stream functions without explicit clock types.

This module is not meant to be used externally,
and is thus not exported from 'FRP.Rhine'.
-}
module FRP.Rhine.Reactimation.ClockErasure where

-- base
import Control.Monad (join)

-- automaton
import Data.Automaton.Trans.Reader
import Data.Stream.Result (Result (..))

-- rhine
import FRP.Rhine.ClSF hiding (runReaderS)
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Clock.Util
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Schedule (In, Out, SequentialClock)

{- | Run a clocked signal function as an automaton,
   accepting the timestamps and tags as explicit inputs.
-}
eraseClockClSF ::
  (Monad m, Clock m cl) =>
  ClockProxy cl ->
  ClSF m cl a b ->
  Automaton m (Time cl, Tag cl, a) b
eraseClockClSF proxy clsf = proc (time, tag, a) -> do
  timeInfo <- genTimeInfo proxy -< (time, tag)
  runReaderS clsf -< (timeInfo, a)
{-# INLINE eraseClockClSF #-}

-- Andras' trick: Encode in the domain
newtype SN m cl a b = SN { getSN :: Automaton m (Time cl, Tag cl, Maybe a) (Maybe b) }
eraseClockSN = getSN

instance GetClockProxy cl => ToClockProxy (SN m cl a b) where
  type Cl (SN m cl a b) = cl

-- A synchronous signal network is run by erasing the clock from the clocked signal function.
synchronous :: forall cl m a b .     ( cl ~ In cl, cl ~ Out cl, Monad m, Clock m cl, GetClockProxy cl) =>
    ClSF m cl a b ->
    SN   m cl a b

synchronous clsf = SN $ proc (time, tag, Just a) -> do
  b <- eraseClockClSF (getClockProxy @cl) clsf -< (time, tag, a)
  returnA -< Just b
{-# INLINE synchronous #-}

-- A sequentially composed signal network may either be triggered in its first component,
-- or its second component. In either case,
-- the resampling buffer (which connects the two components) may be triggered,
-- but only if the outgoing clock of the first component ticks,
-- or the incoming clock of the second component ticks.
sequential ::     ( Clock m clab, Clock m clcd
    , Clock m (Out clab), Clock m (Out clcd)
    , Clock m (In  clab), Clock m (In  clcd)
    , GetClockProxy clab, GetClockProxy clcd
    , Time clab ~ Time clcd
    , Time clab ~ Time (Out clab)
    , Time clcd ~ Time (In  clcd), Monad m
    ) =>
    SN               m      clab            a b     ->
    ResamplingBuffer m (Out clab) (In clcd)   b c   ->
    SN               m                clcd      c d ->
    SN m (SequentialClock   clab      clcd) a     d
sequential sn1 resBuf sn2  = SN $
  let
    proxy1 = toClockProxy sn1
    proxy2 = toClockProxy sn2
   in
    proc (time, tag, maybeA) -> do
      resBufIn <- case tag of
        Left tagL -> do
          maybeB <- eraseClockSN sn1 -< (time, tagL, maybeA)
          returnA -< Left <$> ((time,,) <$> outTag proxy1 tagL <*> maybeB)
        Right tagR -> do
          returnA -< Right . (time,) <$> inTag proxy2 tagR
      maybeC <- mapMaybeS $ eraseClockResBuf (outProxy proxy1) (inProxy proxy2) resBuf -< resBufIn
      case tag of
        Left _ -> do
          returnA -< Nothing
        Right tagR -> do
          eraseClockSN sn2 -< (time, tagR, join maybeC)
{-# INLINE sequential #-}

parallel snL snR = SN $ proc (time, tag, maybeA) -> do
  case tag of
    Left tagL -> eraseClockSN snL -< (time, tagL, maybeA)
    Right tagR -> eraseClockSN snR -< (time, tagR, maybeA)
{-# INLINE parallel #-}

postcompose sn clsf = SN $
  let
    proxy = toClockProxy sn
   in
    proc input@(time, tag, _) -> do
      bMaybe <- eraseClockSN sn -< input
      mapMaybeS $ eraseClockClSF (outProxy proxy) clsf -< (time,,) <$> outTag proxy tag <*> bMaybe
{-# INLINE postcompose #-}

precompose clsf sn = SN $
  let
    proxy = toClockProxy sn
   in
    proc (time, tag, aMaybe) -> do
      bMaybe <- mapMaybeS $ eraseClockClSF (inProxy proxy) clsf -< (time,,) <$> inTag proxy tag <*> aMaybe
      eraseClockSN sn -< (time, tag, bMaybe)
{-# INLINE precompose #-}

feedbackSN ResamplingBuffer {buffer, put, get} sn = SN $
  let
    proxy = toClockProxy sn
   in
    feedback buffer $ proc ((time, tag, aMaybe), buf) -> do
      (cMaybe, buf') <- case inTag proxy tag of
        Nothing -> do
          returnA -< (Nothing, buf)
        Just tagIn -> do
          timeInfo <- genTimeInfo (inProxy proxy) -< (time, tagIn)
          Result buf' c <- arrM $ uncurry get -< (timeInfo, buf)
          returnA -< (Just c, buf')
      bdMaybe <- eraseClockSN sn -< (time, tag, (,) <$> aMaybe <*> cMaybe)
      case (,) <$> outTag proxy tag <*> bdMaybe of
        Nothing -> do
          returnA -< (Nothing, buf')
        Just (tagOut, (b, d)) -> do
          timeInfo <- genTimeInfo (outProxy proxy) -< (time, tagOut)
          buf'' <- arrM $ uncurry $ uncurry put -< ((timeInfo, d), buf')
          returnA -< (Just b, buf'')
{-# INLINE feedbackSN #-}

firstResampling sn buf = SN $
  let
    proxy = toClockProxy sn
   in
    proc (time, tag, acMaybe) -> do
      bMaybe <- eraseClockSN sn -< (time, tag, fst <$> acMaybe)
      let
        resBufInput = case (inTag proxy tag, outTag proxy tag, snd <$> acMaybe) of
          (Just tagIn, _, Just c) -> Just $ Left (time, tagIn, c)
          (_, Just tagOut, _) -> Just $ Right (time, tagOut)
          _ -> Nothing
      dMaybe <- mapMaybeS $ eraseClockResBuf (inProxy proxy) (outProxy proxy) buf -< resBufInput
      returnA -< (,) <$> bMaybe <*> join dMaybe
{-# INLINE firstResampling #-}

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
  ResBuf m cl1 cl2 a b ->
  Automaton m (Either (Time cl1, Tag cl1, a) (Time cl2, Tag cl2)) (Maybe b)
eraseClockResBuf proxy1 proxy2 ResamplingBuffer {buffer, put, get} = feedback buffer $ proc (input, resBuf) -> do
  case input of
    Left (time1, tag1, a) -> do
      timeInfo1 <- genTimeInfo proxy1 -< (time1, tag1)
      resBuf' <- arrM (uncurry $ uncurry put) -< ((timeInfo1, a), resBuf)
      returnA -< (Nothing, resBuf')
    Right (time2, tag2) -> do
      timeInfo2 <- genTimeInfo proxy2 -< (time2, tag2)
      Result resBuf' b <- arrM (uncurry get) -< (timeInfo2, resBuf)
      returnA -< (Just b, resBuf')
{-# INLINE eraseClockResBuf #-}
