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
import Control.Monad.Trans.MSF.Maybe
import Control.Monad.Trans.MSF.Writer

-- rhine
import FRP.Rhine hiding (try, once_, throwMaybe)


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
      reactimate $ runWriterS runningClock >>> proc (w', (time, tag_)) ->
        arrM (putMVar mvar) -< ((time, leftright tag_), w')

-- | Schedule in the @ExceptT e IO@ monad.
--   Whenever one clock encounters an exception in 'ExceptT',
--   this exception is thrown in the other clock's 'ExceptT' layer as well,
--   and in the schedule's (i.e. in the main clock's) thread.
concurrentlyExcept
  :: ( Clock (ExceptT e IO) cl1
     , Clock (ExceptT e IO) cl2
     , Time cl1 ~ Time cl2
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
      initialised <- runExceptT $ initClock cl
      case initialised of
        Right (runningClock, initTime) -> do
          putMVar iMVar $ Right initTime
          Left e <- runExceptT $ reactimate $ runningClock >>> proc (td, tag2) -> do
            arrM (lift . putMVar mvar)              -< Right (td, leftright tag2)
            me <- arrM_ (lift $ readIORef errorref) -< ()
            _  <- throwMaybe                        -< me
            returnA -< ()
          putMVar mvar $ Left e -- Either throw own exception or acknowledge the exception from the other clock
        Left e -> void $ putMVar iMVar $ Left e
    catchAndDrain mvar initScheduleAction = catchE initScheduleAction $ \e -> do
      _ <- reactimate $ (arrM_ $ ExceptT $ takeMVar mvar) >>> arr (const ()) -- Drain the mvar until the other clock acknowledges the exception
      throwE e

-- | As 'concurrentlyExcept', with a single possible exception value.
concurrentlyMaybe
  :: ( Clock (MaybeT IO) cl1
     , Clock (MaybeT IO) cl2
     , Time cl1 ~ Time cl2
     )
  => Schedule (MaybeT IO) cl1 cl2
concurrentlyMaybe = Schedule $ \cl1 cl2 -> initSchedule
  (hoistSchedule exceptTIOToMaybeTIO concurrentlyExcept)
    (HoistClock cl1 maybeTIOToExceptTIO)
    (HoistClock cl2 maybeTIOToExceptTIO)
      where
        exceptTIOToMaybeTIO :: ExceptT () IO a -> MaybeT IO a
        exceptTIOToMaybeTIO = exceptToMaybeT
        maybeTIOToExceptTIO :: MaybeT IO a -> ExceptT () IO a
        maybeTIOToExceptTIO = maybeToExceptT ()
