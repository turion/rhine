module Control.Monad.Schedule.Class where


-- base
import Control.Arrow
import Control.Concurrent
import Data.Either
import Data.Foldable (forM_)
import Data.List.NonEmpty as N
import Data.Function

-- transformers
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

-- TODO: Return value could also be m (NonEmpty a, [m a]), if several finish at the same time.
-- | 'Monad's in which actions can be scheduled concurrently.
class MonadSchedule m where
  -- | Run the actions concurrently,
  --   and return the result of the first finisher,
  --   together with completions for the unfinished actions.
  schedule :: NonEmpty (m a) -> m (a, [m a])

{- |
Fork all actions concurrently in separate threads and wait for the first one to complete.

Many clocks tick at nondeterministic times
(such as event sources),
and it is thus impossible to schedule them deterministically
with most other clocks.
Using concurrency, they can still be scheduled with all clocks in 'IO',
by running the clocks in separate threads.
-}
instance MonadSchedule IO where
  schedule as = do
    var <- newEmptyMVar
    forM_ as $ \action -> forkIO $ putMVar var =<< action
    a <- takeMVar var
    let remaining = replicate (N.length as - 1) $ takeMVar var
    return (a, remaining)

-- TODO Needs dependency
-- instance MonadSchedule STM where

-- | Write in the order of scheduling:
--   The first action to return writes first.
instance (Monoid w, Functor m, MonadSchedule m) => MonadSchedule (WriterT w m) where
  schedule = N.map runWriterT
    >>> schedule
    >>> fmap (assoc >>> first (second $ fmap WriterT))
    >>> WriterT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)

-- | Broadcast the same environment to all actions.
--   The continuations keep this initial environment.
instance (Monad m, MonadSchedule m) => MonadSchedule (ReaderT r m) where
  schedule actions = ReaderT $ \r
    -> N.map (flip runReaderT r) actions
    & schedule
    & fmap (second $ fmap lift)

-- TODO MaybeT? ExceptT??

-- | Runs two values in a 'MonadSchedule' concurrently
--   and returns the first one that yields a value
--   and a continuation for the other value.
race
  :: (Functor m, MonadSchedule m)
  => m a -> m b
  -> m (Either (a, m b) (m a, b))
race aM bM = recoverResult <$> schedule ((Left <$> aM) :| [Right <$> bM])
  where
    recoverResult (Left  a, [bM']) = Left (a, fromRight e <$> bM')
    recoverResult (Right b, [aM']) = Right (fromLeft e <$> aM', b)
    recoverResult _ = e
    e = error "race: Internal error"

-- | Runs both schedules concurrently and returns their results at the end.
async
  :: (Monad m, MonadSchedule m)
  => m  a -> m b
  -> m (a,     b)
async aSched bSched = do
  ab <- race aSched bSched
  case ab of
    Left  (a, bCont) -> do
      b <- bCont
      return (a, b)
    Right (aCont, b) -> do
      a <- aCont
      return (a, b)
