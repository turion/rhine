{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module FRP.Rhine.Schedule.Concurrently where

-- base
import Control.Concurrent
import Control.Monad (void)
import Data.IORef

-- transformers
import Control.Monad.Trans.Class

-- dunai
import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.MSF.Writer

-- rhine
import FRP.Rhine hiding (try, once_, throwMaybe)


-- | Runs two clocks in separate GHC threads
--   and collects the results in the foreground thread.
--   Caution: The data processing will still happen in the same thread
--   (since data processing and scheduling are separated concerns).
concurrently
  :: ( Clock IO cl1, Clock IO cl2
     , TimeDomainOf cl1 ~ TimeDomainOf cl2
     )
  => Schedule IO cl1 cl2
concurrently = Schedule $ \cl1 cl2 -> do
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

concurrentlyWriter
  :: ( Monoid w
     , Clock (WriterT w IO) cl1
     , Clock (WriterT w IO) cl2
     , TimeDomainOf cl1 ~ TimeDomainOf cl2
     )
  => Schedule (WriterT w IO) cl1 cl2
concurrentlyWriter = Schedule $ \cl1 cl2 -> do
  iMVar <- lift newEmptyMVar
  mvar  <- lift newEmptyMVar
  _ <- lift $ forkIO $ do
    ((runningClock, initTime), w) <- runWriterT $ startClock cl1
    putMVar iMVar (initTime, w)
    reactimate $ proc _ -> do
      (wL, (td, tag_)) <- runWriterS runningClock -< ()
      arrM (putMVar mvar)                        -< ((td, Left tag_), wL)
  -- TODO It's a shame I can't refactor these two blocks with a let binding
  _ <- lift $ forkIO $ do
    ((runningClock, initTime), w) <- runWriterT $ startClock cl2
    putMVar iMVar (initTime, w)
    reactimate $ proc _ -> do
      (wR, (td, tag_)) <- runWriterS runningClock -< ()
      arrM (putMVar mvar)                        -< ((td, Right tag_), wR)
  -- The first clock to be initialised sets the first time stamp
  (initTime, w1) <- lift $ takeMVar iMVar
   -- Initialise the second clock
  (_       , w2) <- lift $ takeMVar iMVar
  tell w1
  tell w2
  return (arrM_ (WriterT $ takeMVar mvar), initTime)

-- | Schedule in the @ExceptT e IO@ monad.
--   Whenever one clock encounters an exception in 'ExceptT',
--   this exception is thrown in the other clock's 'ExceptT' layer as well,
--   and in the schedule's (i.e. in the main clock's) thread.
concurrentlyExcept
  :: ( Clock (ExceptT e IO) cl1
     , Clock (ExceptT e IO) cl2
     , TimeDomainOf cl1 ~ TimeDomainOf cl2
     )
  => Schedule (ExceptT e IO) cl1 cl2
concurrentlyExcept = Schedule $ \cl1 cl2 -> do
  (iMVar, mvar, errorref) <- lift $ do
    iMVar <- newEmptyMVar -- The initialisation time is transferred over this variable. It's written to twice.
    mvar  <- newEmptyMVar -- The ticks and exceptions are transferred over this variable. It receives two 'Left' values in total.
    errorref <- newIORef Nothing -- Used to broadcast the exception to both clocks
    _ <- launchSubThread cl1 Left  iMVar mvar errorref
    _ <- launchSubThread cl2 Right iMVar mvar errorref
    return (iMVar, mvar, errorref)
  catchAndDrain mvar $ do
    initTime <- ExceptT $ takeMVar iMVar -- The first clock to be initialised sets the first time stamp
    _        <- ExceptT $ takeMVar iMVar -- Initialise the second clock
    let runningSchedule = arrM_ $ do
          eTick <- lift $ takeMVar mvar
          case eTick of
            Right tick -> return tick
            Left e     -> do
              lift $ writeIORef errorref $ Just e -- Broadcast the exception to both clocks
              throwE e
    return (runningSchedule, initTime)
  where
    launchSubThread cl leftright iMVar mvar errorref = forkIO $ do
      started <- runExceptT $ startClock cl
      case started of
        Right (runningClock, initTime) -> do
          putMVar iMVar $ Right initTime
          Left e <- runExceptT $ reactimate $ runningClock >>> proc (td, tag2) -> do
            arrM (lift . putMVar mvar)            -< Right (td, leftright tag2)
            me <- arrM_ (lift $ readIORef errorref) -< ()
            _  <- throwMaybe               -< me
            returnA -< ()
          putMVar mvar $ Left e -- Either throw own exception or acknowledge the exception from the other clock
        Left e -> void $ putMVar iMVar $ Left e
    catchAndDrain mvar startScheduleAction = catchE startScheduleAction $ \e -> do
      _ <- reactimate $ (arrM_ $ ExceptT $ takeMVar mvar) >>> arr (const ()) -- Drain the mvar until the other clock acknowledges the exception
      throwE e
