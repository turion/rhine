{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Asynchronicity implementation using 'MVar's and free monads.
module Control.Monad.Schedule.FreeAsync (
  -- * 'FreeAsyncT'
  FreeAsyncT (..),
  FreeAsync,
  freeAsync,
  asyncMVar,
  runFreeAsync,
  runFreeAsyncT,

  -- * Concurrent 'Applicative' interface
  ConcurrentlyT (..),
  Concurrently,
  concurrently,
  concurrentlyMVar,
  lift',
  runConcurrentlyT,
  runConcurrently,
)
where

-- base
import Control.Arrow (second, (>>>))
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, tryTakeMVar, yield)
import Control.Monad.IO.Class
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..), appendList, toList)

-- transformers
import Control.Monad.Trans.Class

-- operational
import Control.Monad.Operational (ProgramT, ProgramViewT (..), interpretWithMonadT, singleton, unviewT, viewT)

-- monad-schedule
import Control.Monad.Schedule.Class (MonadSchedule (..), apSchedule)

{- | An 'IO'-like monad with the capability of async/await-style futures.

Synchronous (blocking) computations in this monad can be created using 'lift' and 'liftIO'.
Asynchronous computations that can run in the background are created with 'freeAsync' or 'asyncMVar'.

To leverage the asynchronicity, you can schedule computations with 'MonadSchedule' operations such as 'schedule' or 'race'.

Caution: Composing computations with 'Applicative' or 'Monad' operations like '<*>', '>>=' and @do@-notation
will force all but the final computation in order:
When running @a '*>' b '*>' c@, @b@ will not be started before @a@ has completed.
To start all operations and run them concurrently, use e.g. 'Control.Monad.Schedule.Class.scheduleWith'.
To use an 'Applicative' interface for concurrency, have a look at 'ConcurrentlyT'.
-}
newtype FreeAsyncT m a = FreeAsyncT {getFreeAsyncT :: ProgramT MVar m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

type FreeAsync = FreeAsyncT IO

-- FIXME MFunctor & MMonad instances pending https://github.com/HeinrichApfelmus/operational/pull/28/

-- | Lifts into 'FreeAsyncT' /without/ concurrency. See 'freeAsync'.
instance (MonadIO m) => MonadIO (FreeAsyncT m) where
  liftIO = lift . liftIO

{- | Run an 'IO' computation in the background.

This returns a "promise", or "future", which completes the computation when run.
-}
freeAsync :: (MonadIO m) => IO a -> FreeAsyncT m a
freeAsync action = FreeAsyncT $ do
  var <- liftIO newEmptyMVar
  liftIO $ forkIO $ putMVar var =<< action
  singleton var

-- | Complete all computations and remove the 'FreeAsyncT' layer.
runFreeAsyncT :: (MonadIO m) => FreeAsyncT m a -> m a
runFreeAsyncT = interpretWithMonadT (liftIO . takeMVar) . getFreeAsyncT

-- | Like 'runFreeAsyncT', but specialized to 'IO'.
runFreeAsync :: FreeAsync a -> IO a
runFreeAsync = runFreeAsyncT

{- | Asynchronously await an 'MVar'.

@'asyncMVar' var@ will attempt 'takeMVar' in a way that can be 'schedule'd concurrently with other 'asyncMVar's or 'freeAsync's.
-}
asyncMVar :: MVar a -> FreeAsyncT m a
asyncMVar = FreeAsyncT . singleton

data MVarCont m a = forall b.
  MVarCont
  { mvar :: MVar b
  , cont :: b -> ProgramT MVar m a
  }

embedMVarCont :: (Monad m) => MVarCont m a -> FreeAsyncT m a
embedMVarCont MVarCont {mvar, cont} = FreeAsyncT $ unviewT $ mvar :>>= cont

{- | Concurrently wait for the completion of 'IO' actions.
Has a slight runtime overhead over the direct @'MonadSchedule' 'IO'@ instance, but better fairness.
-}
instance (MonadIO m) => MonadSchedule (FreeAsyncT m) where
  schedule actions = retryForever $ getFreeAsyncT <$> actions
    where
      retryForever :: (MonadIO m) => NonEmpty (ProgramT MVar m a) -> FreeAsyncT m (NonEmpty a, [FreeAsyncT m a])
      retryForever actions = do
        -- Look at all actions
        views <- lift (mapM viewT actions)
        -- Have some of them finished?
        case partitionNonEmpty $ viewToEither <$> views of
          -- All have finished
          Left (as, []) -> return (as, [])
          -- Some have finished, some are waiting for MVars
          Left (as, cont : conts) -> do
            -- Peek at the MVars
            progressed <- lift $ tryProgresses $ cont :| conts
            -- Are some MVars present already?
            case progressed of
              -- Yes, some were present, step the corresponding actions
              Left (actions, conts) -> do
                -- Look at the progressed actions
                views <- lift (mapM viewT actions)
                -- Have some of them returned now?
                case partitionNonEmpty $ viewToEither <$> views of
                  -- Yes. Return those as well
                  Left (as', conts') -> return (as <> as', embedMVarCont <$> (conts ++ conts'))
                  -- No, they are blocked on other MVars now
                  Right conts' -> return (as, embedMVarCont <$> toList conts' <> conts)
              -- All MVars are still blocked
              Right conts -> return (as, embedMVarCont <$> toList conts)
          -- All actions are waiting for MVars
          Right conts -> do
            -- Retry until some MVars get unblocked
            (progressed, conts) <- lift $ retryProgresses conts
            -- Some MVars are unblocked, start over.
            retryForever $ appendList progressed $ getFreeAsyncT . embedMVarCont <$> conts

      viewToEither :: ProgramViewT MVar m a -> Either a (MVarCont m a)
      viewToEither (Return a) = Left a
      viewToEither (mvar :>>= cont) = Right MVarCont {mvar, cont}

      partitionNonEmpty :: NonEmpty (Either a b) -> Either (NonEmpty a, [b]) (NonEmpty b)
      partitionNonEmpty (Left a :| abs) = let (as, bs) = partitionEithers abs in Left (a :| as, bs)
      partitionNonEmpty (Right b :| abs) = case partitionEithers abs of
        ([], bs) -> Right $ b :| bs
        (a : as, bs) -> Left (a :| as, b : bs)

      tryProgress :: (MonadIO m) => MVarCont m a -> m (Either (ProgramT MVar m a) (MVarCont m a))
      tryProgress mvarcont@MVarCont {mvar, cont} = do
        result <- liftIO $ tryTakeMVar mvar
        return $ maybe (Right mvarcont) (Left . cont) result

      tryProgresses :: (MonadIO m) => NonEmpty (MVarCont m a) -> m (Either (NonEmpty (ProgramT MVar m a), [MVarCont m a]) (NonEmpty (MVarCont m a)))
      tryProgresses conts = do
        result <- partitionNonEmpty <$> mapM tryProgress conts
        case result of
          Left (progressed, []) -> return $ Left (progressed, [])
          Left (progressed, cont : conts) -> do
            inner <- tryProgresses $ cont :| conts
            case inner of
              Left (progressed', finalConts) -> return $ Left (progressed <> progressed', finalConts)
              Right finalConts -> return $ Left (progressed, toList finalConts)
          Right conts -> return $ Right conts

      retryProgresses :: (MonadIO m) => NonEmpty (MVarCont m a) -> m (NonEmpty (ProgramT MVar m a), [MVarCont m a])
      retryProgresses conts = do
        result <- tryProgresses conts
        case result of
          Left progress -> return progress
          Right _ -> do
            liftIO $ yield >> threadDelay 100
            retryProgresses conts

{- | Like 'FreeAsyncT', but leverages concurrency in the 'Applicative' interface.

The central difference to 'FreeAsyncT' is the 'Applicative' instance:
@concurrently a *> concurrently b *> concurrently c@ will launch all three actions immediately
and return when all actions have completed.
On the other hand, @concurrently a >>= f@ has to compute sequentially.

For more readable syntax, it can be useful to switch the @ApplicativeDo@ extension on.

The downside of this 'Applicative' instance is that 'ConcurrentlyT' can't be an instance of 'MonadTrans'.
As a drop-in replacement, the function 'lift'' is supplied.

Caution: To lift an 'IO' action concurrently, you need to use 'concurrently' and not 'liftIO'.
-}
newtype ConcurrentlyT m a = ConcurrentlyT {getConcurrentlyT :: FreeAsyncT m a}
  deriving newtype (Functor, Monad, MonadIO)

type Concurrently = ConcurrentlyT IO

{- | Lift an 'IO' action such that it can be run concurrently.

See 'freeAsync'.
-}
concurrently :: (MonadIO m) => IO a -> ConcurrentlyT m a
concurrently = ConcurrentlyT . freeAsync

-- | Like 'asyncMVar'
concurrentlyMVar :: MVar a -> ConcurrentlyT m a
concurrentlyMVar = ConcurrentlyT . asyncMVar

{- | Lift a computation to 'ConcurrentlyT'.

This replaces the missing 'MonadTrans' instance.

Caution: Computations lifted with this function cannot be scheduled concurrently!
If this is your intention, 'concurrently' needs to be used instead.
-}
lift' :: (Monad m) => m a -> ConcurrentlyT m a
lift' = ConcurrentlyT . lift

-- | Run a 'ConcurrentlyT' computation to completion, removing the newtype layers.
runConcurrentlyT :: (MonadIO m) => ConcurrentlyT m a -> m a
runConcurrentlyT = runFreeAsyncT . getConcurrentlyT

-- | Like 'runConcurrently', but specialised to 'IO'.
runConcurrently :: Concurrently a -> IO a
runConcurrently = runConcurrentlyT

instance (MonadIO m) => Applicative (ConcurrentlyT m) where
  pure = ConcurrentlyT . pure
  (<*>) = apSchedule

-- | Like 'FreeAsyncT', but executes actions composed via the 'Applicative' interface concurrently.
instance (MonadIO m) => MonadSchedule (ConcurrentlyT m) where
  schedule =
    fmap getConcurrentlyT
      >>> schedule
      >>> fmap (second (map ConcurrentlyT))
      >>> ConcurrentlyT
