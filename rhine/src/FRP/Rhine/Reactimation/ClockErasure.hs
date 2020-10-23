{- |
Translate clocked signal processing components to stream functions without explicit clock types.

This module is not meant to be used externally,
and is thus not exported from 'FRP.Rhine'.
-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module FRP.Rhine.Reactimation.ClockErasure where

-- base
import Control.Monad (join)
import Data.Maybe (fromJust, fromMaybe)

-- dunai
import Control.Monad.Trans.MSF.Reader
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Clock.Util
import FRP.Rhine.ClSF hiding (runReaderS)
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Schedule
import FRP.Rhine.SN
import Control.Monad.Schedule.Class

-- | Run a clocked signal function as a monadic stream function,
--   accepting the timestamps and tags as explicit inputs.
eraseClockClSF
  :: (Monad m, Clock m cl)
  => ClockProxy cl
  -> Time cl
  -> ClSF m cl a b
  -> MSF m (Time cl, Tag cl, a) b
eraseClockClSF proxy initialTime clsf = proc (time, tag, a) -> do
  timeInfo <- genTimeInfo proxy initialTime -< (time, tag)
  runReaderS clsf                           -< (timeInfo, a)

{- | Run a signal network as a monadic stream function.

Depending on the incoming clock @cla@,
input data may need to be present or not,
but this is not up to the caller to decide.
Therefore, the input must always be present,
even if it may not be used.

Depending on the outgoing clock @clb@,
output data may or may not be generated.
-}
eraseClockSN
  :: ( Monad m, MonadSchedule m )
  => SN m cla clb a b
  -> m (MSF m a (Maybe b))

{-
eraseClockSN (Synchronous clsf cl) = do
  (runningClock, initialTime) <- initClock cl
  -- let erasedClockClSF = runReaderS clsf >>> arr Just
  -- runningClock >>> genTimeInfo getClockProxy initialTime)
  return $ proc a -> do
    time <- runningClock -< ()
    timeInfo <- genTimeInfo getClockProxy initialTime -< time
    b <- runReaderS clsf -< (timeInfo, a)
    returnA -< Just b
-}

{-
eraseClockSN sn = do
  (erasedClockSN, runningClock) <- runWithClockSN sn
  return $ proc a -> do
    timeInfo <- runningClock -< ()
    (_, bMaybe) <- erasedClockSN -< (timeInfo, a)
    returnA -< bMaybe
-}
eraseClockSN sn = (arr (fmap snd) <<<) <$> runWithClockSN sn

runWithClockSN
  :: ( Monad m, MonadSchedule m )
  => SN m cla clb a b
  -> m (MSF m a (Maybe (TimeInfo clb, b)))
-- A synchronous signal network is run by erasing the clock from the clocked signal function.
runWithClockSN (Synchronous clsf cl) = do
  (runningClock, initialTime) <- initClock cl
  return $ proc a -> do
    timeInfo <- genTimeInfo getClockProxy initialTime <<< runningClock -< ()
    b <- runReaderS clsf -< (timeInfo, a)
    returnA -< Just (timeInfo, b)

-- A sequentially composed signal network may either be triggered in its first component,
-- or its second component. In either case,
-- the resampling buffer (which connects the two components) may be triggered,
-- but only if the outgoing clock of the first component ticks,
-- or the incoming clock of the second component ticks.
runWithClockSN (Sequential sn resBuf cl clsf) = do
  erasedClockSN <- runWithClockSN sn
  (runningClock, initialTime) <- initClock cl
  return $ proc a -> do
    snOrCl <- scheduleList
      [ arr Left  <<< erasedClockSN -- TODO The slight problem here is that this mixes delays in the SN with delays in the clock, potentially upsetting the scheduling.
        -- Potential solution: Separate clock and SN and create both again
      , arr Right <<< genTimeInfo getClockProxy initialTime <<< runningClock <<< arr (const ())
      ] -< a
    resBufIn <- case snOrCl of
      Left  timeInfoAndB -> do
        returnA -< Left <$> timeInfoAndB
      Right timeInfo -> do
        returnA -< Just $ Right timeInfo
    maybeC <- mapMaybeS $ eraseClockResBuf resBuf -< resBufIn
    case (snOrCl, maybeC) of
      (Left _, _)    -> do
        returnA -< Nothing
      (Right timeInfo, Just (Just c)) -> do
        d <- runReaderS clsf -< (timeInfo, c) -- (time, tagR, join maybeC)
        returnA -< Just (timeInfo, d)
      _ -> returnA -< internalError "Resampling buffer and rightmost clock returned incoherent values"

runWithClockSN (Parallel snL snR) = do
  erasedClockSNL <- runWithClockSN snL
  erasedClockSNR <- runWithClockSN snR
  return $ scheduleList
    [ arr (fmap $ first $ retag Left) <<< erasedClockSNL
    , arr (fmap $ first $ retag Right) <<< erasedClockSNR
    ]

internalError :: String -> a
internalError msg = error $ "Internal error in eraseClockSN: " ++ msg

-- | Translate a resampling buffer into a monadic stream function.
--
--   The input decides whether the buffer is to accept input or has to produce output.
--   (In the latter case, only time information is provided.)
eraseClockResBuf
  :: ( Monad m
     , Clock m cl1, Clock m cl2
     , Time cl1 ~ Time cl2
     )
  => ResBuf m cl1 cl2 a b
  -> MSF m (Either (TimeInfo cl1, a) (TimeInfo cl2)) (Maybe b)
eraseClockResBuf resBuf0 = feedback resBuf0 $ proc (input, resBuf) -> do
  case input of
    Left (timeInfo1, a) -> do
      resBuf'   <- arrM (uncurry $ uncurry put)     -< ((resBuf, timeInfo1), a)
      returnA                                       -< (Nothing, resBuf')
    Right timeInfo2 -> do
      (b, resBuf') <- arrM (uncurry get)             -< (resBuf, timeInfo2)
      returnA                                        -< (Just b, resBuf')
