{- |
Translate clocked signal processing components to stream functions without explicit clock types.

This module is not meant to be used externally,
and is thus not exported from 'FRP.Rhine'.
-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module FRP.Rhine.Reactimation.ClockErasure where

-- base
import Control.Monad (join, void)
import Data.Data

-- dunai
import Control.Monad.Trans.MSF.Reader
import Data.MonadicStreamFunction hiding (Feedback)

-- rhine
import FRP.Rhine.Clock hiding (Feedback)
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Clock.Util
import FRP.Rhine.ClSF hiding (Feedback, runReaderS)
import FRP.Rhine.ResamplingBuffer hiding (Feedback)
import FRP.Rhine.SN
import FRP.Rhine.Schedule (In, Out)
import FRP.Rhine.Type (Rhine)

-- | Run a clocked signal function as a monadic stream function,
--   accepting the timestamps and tags as explicit inputs.
eraseClockClSF
  :: ( Monad m, Clock m cl
     , Data (Tag cl), Data (Time cl)
     )
  => ClockProxy cl -> Time cl
  -> ClSF m cl a b
  -> MSF m (Time cl, Tag cl, a) b
eraseClockClSF proxy initialTime clsf = proc (time, tag, a) -> do
  timeInfo <- genTimeInfo proxy initialTime -< (time, tag)
  runReaderS clsf                           -< (timeInfo, a)

-- | Run a signal network as a monadic stream function.
--
--   Depending on the incoming clock,
--   input data may need to be provided,
--   and depending on the outgoing clock,
--   output data may be generated.
--   There are thus possible invalid inputs,
--   which 'eraseClockSN' does not gracefully handle.
eraseClockSN
  :: ( Monad m, Clock m cl, GetClockProxy cl
     , Data (Tag cl), Data (Time cl)
     )
  => Time cl
  -> SN m cl a b
  -> MSF m (Time cl, Tag cl, Maybe a) (Maybe b)

-- A synchronous signal network is run by erasing the clock from the clocked signal function.
eraseClockSN initialTime sn@(Synchronous clsf) = proc (time, tag, Just a) -> do
  b <- eraseClockClSF (toClockProxy sn) initialTime clsf -< (time, tag, a)
  returnA                                                -< Just b

-- A sequentially composed signal network may either be triggered in its first component,
-- or its second component. In either case,
-- the resampling buffer (which connects the two components) may be triggered,
-- but only if the outgoing clock of the first component ticks,
-- or the incoming clock of the second component ticks.
eraseClockSN initialTime (Sequential sn1 resBuf sn2) =
  let
    proxy1 = toClockProxy sn1
    proxy2 = toClockProxy sn2
  in proc (time, tag, maybeA) -> do
  resBufIn <- case tag of
    Left  tagL -> do
      maybeB <- eraseClockSN initialTime sn1 -< (time, tagL, maybeA)
      returnA -< Left <$> ((time, , ) <$> outTag proxy1 tagL <*> maybeB)
    Right tagR -> do
      returnA -< Right . (time, ) <$> inTag proxy2 tagR
  maybeC <- mapMaybeS $ eraseClockResBuf (outProxy proxy1) (inProxy proxy2) initialTime resBuf -< resBufIn
  case tag of
    Left  _    -> do
      returnA -< Nothing
    Right tagR -> do
      eraseClockSN initialTime sn2 -< (time, tagR, join maybeC)

eraseClockSN initialTime (Parallel snL snR) = proc (time, tag, maybeA) -> do
  case tag of
    Left  tagL -> eraseClockSN initialTime snL -< (time, tagL, maybeA)
    Right tagR -> eraseClockSN initialTime snR -< (time, tagR, maybeA)

eraseClockSN initialTime (Postcompose sn clsf) =
  let
    proxy = toClockProxy sn
  in proc input@(time, tag, _) -> do
  bMaybe <- eraseClockSN initialTime sn -< input
  mapMaybeS $ eraseClockClSF (outProxy proxy) initialTime clsf -< (time, , ) <$> outTag proxy tag <*> bMaybe

eraseClockSN initialTime (Precompose clsf sn) =
  let
    proxy = toClockProxy sn
  in proc (time, tag, aMaybe) -> do
  bMaybe <- mapMaybeS $ eraseClockClSF (inProxy proxy) initialTime clsf -< (time, , ) <$> inTag proxy tag <*> aMaybe
  eraseClockSN initialTime sn -< (time, tag, bMaybe)

eraseClockSN initialTime (Feedback buf0 sn) =
  let
    proxy = toClockProxy sn
  in feedback buf0 $ proc ((time, tag, aMaybe), buf) -> do
  (cMaybe, buf') <- case inTag proxy tag of
    Nothing -> do
      returnA -< (Nothing, buf)
    Just tagIn -> do
      timeInfo <- genTimeInfo (inProxy proxy) initialTime -< (time, tagIn)
      (c, buf') <- arrM $ uncurry get -< (buf, timeInfo)
      returnA -< (Just c, buf')
  bdMaybe <- eraseClockSN initialTime sn -< (time, tag, (, ) <$> aMaybe <*> cMaybe)
  case (, ) <$> outTag proxy tag <*> bdMaybe of
    Nothing -> do
      returnA -< (Nothing, buf')
    Just (tagOut, (b, d)) -> do
      timeInfo <- genTimeInfo (outProxy proxy) initialTime -< (time, tagOut)
      buf'' <- arrM $ uncurry $ uncurry put -< ((buf', timeInfo), d)
      returnA -< (Just b, buf'')

eraseClockSN initialTime (FirstResampling sn buf) =
  let
    proxy = toClockProxy sn
  in proc (time, tag, acMaybe) -> do
    bMaybe <- eraseClockSN initialTime sn -< (time, tag, fst <$> acMaybe)
    let
      resBufInput = case (inTag proxy tag, outTag proxy tag, snd <$> acMaybe) of
        (Just tagIn, _, Just c) -> Just $ Left (time, tagIn, c)
        (_, Just tagOut, _) -> Just $ Right (time, tagOut)
        _ -> Nothing
    dMaybe <- mapMaybeS $ eraseClockResBuf (inProxy proxy) (outProxy proxy) initialTime buf -< resBufInput
    returnA -< (,) <$> bMaybe <*> join dMaybe

-- | Translate a resampling buffer into a monadic stream function.
--
--   The input decides whether the buffer is to accept input or has to produce output.
--   (In the latter case, only time information is provided.)
eraseClockResBuf
  :: ( Monad m
     , Clock m cl1, Clock m cl2
     , Time cl1 ~ Time cl2
     , Data (Tag cl1), Data (Tag cl2)
     , Data (Time cl1), Data (Time cl2)
     )
  => ClockProxy cl1 -> ClockProxy cl2 -> Time cl1
  -> ResBuf m cl1 cl2 a b
  -> MSF m (Either (Time cl1, Tag cl1, a) (Time cl2, Tag cl2)) (Maybe b)
eraseClockResBuf proxy1 proxy2 initialTime ResamplingBuffer { .. } = feedback resamplingState $ proc (input, resState) -> do
  case input of
    Left (time1, tag1, a) -> do
      timeInfo1 <- genTimeInfo proxy1 initialTime   -< (time1, tag1)
      resState'   <- arrM (uncurry $ uncurry put)   -< ((resState, timeInfo1), a)
      returnA                                       -< (Nothing, resState')
    Right (time2, tag2) -> do
      timeInfo2    <- genTimeInfo proxy2 initialTime -< (time2, tag2)
      (b, resState') <- arrM (uncurry get)           -< (resState, timeInfo2)
      returnA                                        -< (Just b, resState')

eraseClockRhine
  :: ( Monad m, Clock m cl
     , GetClockProxy cl
     , Time cl ~ Time (In  cl)
     , Time cl ~ Time (Out cl)
     )
  => Rhine m cl () () -> m (MSF m () ())
eraseClockRhine Rhine {..} = do
  (runningClock, initTime) <- initClock clock
  return $ eraseClockRunningAndSN runningClock initTime sn

eraseClockRunningAndSN
  :: ( Monad m, Clock m cl
     , GetClockProxy cl
     , Time cl ~ Time (In  cl)
     , Time cl ~ Time (Out cl)
     )
  => RunningClock m (Time cl) (Tag cl) -> Time cl -> SN m cl () () -> MSF m () ()
eraseClockRunningAndSN runningClock initTime sn = proc () -> do
    (time, tag) <- runningClock -< ()
    eraseClockSN initTime sn -< (time, tag, void $ inTag (toClockProxy sn) tag)
    returnA -< ()
