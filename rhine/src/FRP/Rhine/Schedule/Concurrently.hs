{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module FRP.Rhine.Schedule.Concurrently where

-- base
import Control.Concurrent

-- rhine
import FRP.Rhine


-- | Runs two clocks in separate GHC threads
--   and collects the results in the foreground thread.
--   Caution: The data processing will still happen in the same thread
--   (since data processing and scheduling are separated concerns).
concurrently :: (Clock IO cl1, Clock IO cl2, TimeDomainOf cl1 ~ TimeDomainOf cl2) => Schedule IO cl1 cl2
concurrently =  Schedule $ \cl1 cl2 -> do
  iMVar <- newEmptyMVar
  mvar  <- newEmptyMVar
  _ <- forkIO $ do
    (runningClock, initTime) <- startClock cl1
    putMVar iMVar initTime
    reactimate $ runningClock >>> second (arr Left)  >>> arrM (putMVar mvar)
  _ <- forkIO $ do
    (runningClock, initTime) <- startClock cl2
    putMVar iMVar initTime
    reactimate $ runningClock >>> second (arr Right) >>> arrM (putMVar mvar)
  initTime <- takeMVar iMVar -- The first clock to be initialised sets the first time stamp
  _        <- takeMVar iMVar -- Initialise the second clock
  return (arrM_ $ takeMVar mvar, initTime)

-- TODO These threads can't be killed from outside easily since we've lost their ids
-- => make a MaybeT or ExceptT variant
