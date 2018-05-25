{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module FRP.Rhine.Schedule.Concurrently where

-- base
import Control.Concurrent

-- transformers
import Control.Monad.Trans.Class

-- dunai
import Control.Monad.Trans.MSF.Writer

-- rhine
import FRP.Rhine


-- | Runs two clocks in separate GHC threads
--   and collects the results in the foreground thread.
--   Caution: The data processing will still happen in the same thread
--   (since data processing and scheduling are separated concerns).
concurrently
  :: ( Clock IO cl1, Clock IO cl2
     , Time cl1 ~ Time cl2
     )
  => Schedule IO cl1 cl2
concurrently = Schedule $ \cl1 cl2 -> do
  iMVar <- newEmptyMVar
  mvar  <- newEmptyMVar
  _ <- launchSubthread cl1 Left  iMVar mvar
  _ <- launchSubthread cl2 Right iMVar mvar
  initTime <- takeMVar iMVar -- The first clock to be initialised sets the first time stamp
  _        <- takeMVar iMVar -- Initialise the second clock
  return (arrM_ $ takeMVar mvar, initTime)
  where
    launchSubthread cl leftright iMVar mvar = forkIO $ do
      (runningClock, initTime) <- initClock cl
      putMVar iMVar initTime
      reactimate $ runningClock >>> second (arr leftright) >>> arrM (putMVar mvar)
-- TODO These threads can't be killed from outside easily since we've lost their ids
-- => make a MaybeT or ExceptT variant

concurrentlyWriter
  :: ( Monoid w
     , Clock (WriterT w IO) cl1
     , Clock (WriterT w IO) cl2
     , Time cl1 ~ Time cl2
     )
  => Schedule (WriterT w IO) cl1 cl2
concurrentlyWriter = Schedule $ \cl1 cl2 -> do
  iMVar <- lift newEmptyMVar
  mvar  <- lift newEmptyMVar
  _ <- launchSubthread cl1 Left  iMVar mvar
  _ <- launchSubthread cl2 Right iMVar mvar
  -- The first clock to be initialised sets the first time stamp
  (initTime, w1) <- lift $ takeMVar iMVar
   -- Initialise the second clock
  (_       , w2) <- lift $ takeMVar iMVar
  tell w1
  tell w2
  return (arrM_ (WriterT $ takeMVar mvar), initTime)
  where
    launchSubthread cl leftright iMVar mvar = lift $ forkIO $ do
      ((runningClock, initTime), w) <- runWriterT $ initClock cl
      putMVar iMVar (initTime, w)
      reactimate $ proc _ -> do
        (w', (td, tag_)) <- runWriterS runningClock -< ()
        arrM (putMVar mvar)                        -< ((td, leftright tag_), w')
