{- |
This module supplies a general purpose monad transformer
that adds a syntactical "delay", or "waiting" side effect.

This allows for universal and deterministic scheduling of clocks
that implement their waiting actions in 'ScheduleT'.
See 'FRP.Rhine.Schedule.Trans' for more details.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Control.Monad.Schedule where

-- base
import Data.Ord (comparing)
import Control.Arrow (Arrow(second))
import Control.Concurrent
import Control.Category ((>>>))
import Control.Monad (join)
import Data.Functor.Classes
import Data.List.NonEmpty as N

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- free
import Control.Monad.Trans.Free

-- rhine
import Control.Monad.Schedule.Class

-- TODO Implement Time via StateT

{- |
A functor implementing a syntactical "waiting" action.

* 'diff' represents the duration to wait.
* 'a' is the encapsulated value.
-}
data Wait diff a = Wait
  { getDiff :: diff
  , awaited :: a
  }
  deriving (Functor, Eq, Show)

instance Eq diff => Eq1 (Wait diff) where
  liftEq eq (Wait diff1 a) (Wait diff2 b) = diff1 == diff2 && eq a b

-- | Compare by the time difference, regardless of the value.
compareWait :: Ord diff => Wait diff a -> Wait diff a -> Ordering
compareWait = comparing getDiff

{- |
Values in @ScheduleT diff m@ are delayed computations with side effects in 'm'.
Delays can occur between any two side effects, with lengths specified by a 'diff' value.
These delays don't have any semantics, it can be given to them with 'runScheduleT'.
-}
type ScheduleT diff = FreeT (Wait diff)

-- | The side effect that waits for a specified amount.
wait :: Monad m => diff -> ScheduleT diff m ()
wait diff = FreeT $ return $ Free $ Wait diff $ return ()

-- | Supply a semantic meaning to 'Wait'.
--   For every occurrence of @Wait diff@ in the @ScheduleT diff m a@ value,
--   a waiting action is executed, depending on 'diff'.
runScheduleT :: Monad m => (diff -> m ()) -> ScheduleT diff m a -> m a
runScheduleT waitAction = iterT $ \(Wait n ma) -> waitAction n >> ma

-- | Run a 'ScheduleT' value in a 'MonadIO',
--   interpreting the times as milliseconds.
runScheduleIO
  :: (MonadIO m, Integral n)
  => ScheduleT n m a -> m a
runScheduleIO = runScheduleT $ liftIO . threadDelay . (* 1000) . fromIntegral

instance Ord diff => MonadSchedule (Wait diff) where
  schedule waits = let (smallestWait :| waits') = N.sortBy compareWait waits in (, waits') <$> smallestWait

-- | Run each action one step until it is discovered which action(s) are pure, or yield next.
--   If there is a pure action, it is returned,
--   otherwise all actions are shifted to the time when the earliest action yields.
instance (Num diff, Ord diff, Monad m) => MonadSchedule (ScheduleT diff m) where
  schedule
    =   fmap runFreeT
    >>> sequenceA
    >>> fmap (sortBy compareFreeFWait >>> shiftList)
    >>> lift
    >>> join
    >>> fmap (second $ fmap (FreeT . return))
    where
      compareFreeFWait
        :: Ord diff
        => FreeF (Wait diff) a b
        -> FreeF (Wait diff) a b
        -> Ordering
      compareFreeFWait (Pure _) (Pure _) = EQ
      compareFreeFWait (Pure _) (Free _) = LT
      compareFreeFWait (Free _) (Pure _) = GT
      compareFreeFWait (Free wait1) (Free wait2) = compareWait wait1 wait2

      -- Separate pure from free values
      partitionFreeF
        :: [FreeF f a b]
        -> ([a], [f b])
      partitionFreeF [] = ([], [])
      partitionFreeF (Pure a  : xs) = let (as, fbs) = partitionFreeF xs in (a : as, fbs)
      partitionFreeF (Free fb : xs) = let (as, fbs) = partitionFreeF xs in (as, fb : fbs)

      -- Shift a waiting action by some duration
      shift
        :: Num diff
        => diff
        -> Wait diff a
        -> Wait diff a
      shift diff1 (Wait diff2 a) = Wait (diff2 - diff1) a

      -- Shift a list of free actions by the duration of the head
      -- (assuming the list is sorted).
      -- If the head is pure, return it with the remaining actions,
      -- otherwise wait the minimum duration, give the continuation of the head,
      -- and shift the remaining actions by that minimum duration.
      shiftListOnce
        :: Num diff
        => NonEmpty (FreeF (Wait diff) a b)
        -> Either
             (a, [FreeF (Wait diff) a b]) -- Pure value has completed
             (Wait diff (b, [Wait diff b])) -- All values wait
      shiftListOnce actions = case partitionFreeF $ toList actions of
        (a : as, waits) -> Left (a, (Pure <$> as) ++ (Free <$> waits))
        ([], Wait diff cont : waits) -> Right $ Wait diff (cont, shift diff <$> waits)

      -- Repeatedly shift the list by the smallest available waiting duration
      -- until one action returns as pure.
      -- Return its result, together with the remaining free actions.
      shiftList
        :: (Num diff, Ord diff, Monad m)
        => NonEmpty (FreeF (Wait diff) a (ScheduleT diff m a))
        -> ScheduleT diff m (a, [FreeF (Wait diff) a (ScheduleT diff m a)])
      shiftList actions = case shiftListOnce actions of
        Left (a, freefs) -> return (a, freefs)
        Right (Wait diff (cont, waits)) -> do
            wait diff
            unwrap <- lift $ runFreeT cont
            shiftList $ sortBy compareFreeFWait $ unwrap :| (Free <$> waits)

execScheduleT :: Monad m => ScheduleT diff m a -> m (a, [diff])
execScheduleT action = do
  free <- runFreeT action
  case free of
    Pure a -> return (a, [])
    Free (Wait diff cont) -> do
      (a, diffs) <- execScheduleT cont
      return (a, diff : diffs)
