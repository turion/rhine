{- |
Translate clocked signal processing components to stream functions without explicit clock types.

This module is not meant to be used externally,
and is thus not exported from 'FRP.Rhine'.
-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module FRP.Rhine.Reactimation.ClockErasure where

-- base
import Control.Monad (join)

-- dunai
import Control.Monad.Trans.MSF.Reader
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Clock.Util
import FRP.Rhine.ClSF hiding (runReaderS)
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.SN

-- | Run a clocked signal function as a monadic stream function,
--   accepting the timestamps and tags as explicit inputs.
eraseClockClSF
  :: (Monad m, Clock m cl)
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
  :: (Monad m, Clock m cl, GetClockProxy cl)
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
      returnA -< Right <$> (time, ) <$> inTag proxy2 tagR
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

-- | Translate a resampling buffer into a monadic stream function.
--
--   The input decides whether the buffer is to accept input or has to produce output.
--   (In the latter case, only time information is provided.)
eraseClockResBuf
  :: ( Monad m
     , Clock m cl1, Clock m cl2
     , Time cl1 ~ Time cl2
     )
  => ClockProxy cl1 -> ClockProxy cl2 -> Time cl1
  -> ResBuf m cl1 cl2 a b
  -> MSF m (Either (Time cl1, Tag cl1, a) (Time cl2, Tag cl2)) (Maybe b)
eraseClockResBuf proxy1 proxy2 initialTime resBuf0 = feedback resBuf0 $ proc (input, resBuf) -> do
  case input of
    Left (time1, tag1, a) -> do
      timeInfo1 <- genTimeInfo proxy1 initialTime   -< (time1, tag1)
      resBuf'   <- arrM (uncurry $ uncurry put)     -< ((resBuf, timeInfo1), a)
      returnA                                       -< (Nothing, resBuf')
    Right (time2, tag2) -> do
      timeInfo2    <- genTimeInfo proxy2 initialTime -< (time2, tag2)
      (b, resBuf') <- arrM (uncurry get)             -< (resBuf, timeInfo2)
      returnA                                        -< (Just b, resBuf')
