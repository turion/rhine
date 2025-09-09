{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

{- |
This module supplies a general purpose monad transformer
that adds a syntactical "delay", or "waiting" side effect.
-}
module Control.Monad.Schedule.Trans where

-- base
import Control.Concurrent
import Data.Functor.Classes
import Data.Functor.Identity
import Data.List (partition)
import Data.List.NonEmpty as N hiding (partition)
import Data.Ord (comparing)

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- free
import Control.Monad.Trans.Free

-- time-domain
import Data.TimeDomain

-- monad-schedule
import Control.Monad.Schedule.Class

-- TODO Implement Time via StateT

-- * Waiting action

-- | A functor implementing a syntactical "waiting" action.
data Wait diff a = Wait
  { getDiff :: diff
  -- ^ The duration to wait.
  , awaited :: a
  -- ^ The encapsulated value.
  }
  deriving (Functor, Eq, Show)

instance (Eq diff) => Eq1 (Wait diff) where
  liftEq eq (Wait diff1 a) (Wait diff2 b) = diff1 == diff2 && eq a b

{- | Compare by the time difference, regardless of the value.

  Note that this would not give a lawful 'Ord' instance since we do not compare the @a@.
-}
compareWait :: (Ord diff) => Wait diff a -> Wait diff a -> Ordering
compareWait = comparing getDiff

-- * 'ScheduleT'

{- |
Values in @ScheduleT diff m@ are delayed computations with side effects in 'm'.
Delays can occur between any two side effects, with lengths specified by a 'diff' value.
These delays don't have any semantics, it can be given to them with 'runScheduleT'.
-}
type ScheduleT diff = FreeT (Wait diff)

type Schedule diff = ScheduleT diff Identity

-- | The side effect that waits for a specified amount.
wait :: (Monad m) => diff -> ScheduleT diff m ()
wait diff = FreeT $ return $ Free $ Wait diff $ return ()

{- | Supply a semantic meaning to 'Wait'.
  For every occurrence of @Wait diff@ in the @ScheduleT diff m a@ value,
  a waiting action is executed, depending on 'diff'.
-}
runScheduleT :: (Monad m) => (diff -> m ()) -> ScheduleT diff m a -> m a
runScheduleT waitAction = iterT $ \(Wait n ma) -> waitAction n >> ma

{- | Run a 'ScheduleT' value in a 'MonadIO',
  interpreting the times as milliseconds.
-}
runScheduleIO ::
  (MonadIO m, Integral n) =>
  ScheduleT n m a ->
  m a
runScheduleIO = runScheduleT $ liftIO . threadDelay . (* 1000) . fromIntegral

{- | Formally execute all waiting actions,
  returning the final value and all moments when the schedule would have waited.
-}
execScheduleT :: (Monad m) => ScheduleT diff m a -> m (a, [diff])
execScheduleT action = do
  free <- runFreeT action
  case free of
    Pure a -> return (a, [])
    Free (Wait diff cont) -> do
      (a, diffs) <- execScheduleT cont
      return (a, diff : diffs)

instance (Ord diff) => MonadSchedule (Wait diff) where
  schedule waits = let (smallestWait :| waits') = N.sortBy compareWait waits in (,waits') . pure <$> smallestWait

isZero :: (Eq diff, TimeDifference diff) => diff -> Bool
isZero diff = diff `difference` diff == diff

{- | Run each action one step until it is discovered which action(s) are pure, or yield next.
  If there is a pure action, it is returned,
  otherwise all actions are shifted to the time when the earliest action yields.
-}
instance (Ord diff, TimeDifference diff, Monad m, MonadSchedule m) => MonadSchedule (ScheduleT diff m) where
  schedule actions = do
    (frees, delayed) <- lift $ schedule $ runFreeT <$> actions
    shiftList (sortBy compareFreeFWait frees) $ FreeT <$> delayed
    where
      -- We disregard the inner values @a@ and @b@,
      -- thus this is not an 'Ord' instance.
      compareFreeFWait ::
        (Ord diff) =>
        FreeF (Wait diff) a b ->
        FreeF (Wait diff) a b ->
        Ordering
      compareFreeFWait (Pure _) (Pure _) = EQ
      compareFreeFWait (Pure _) (Free _) = LT
      compareFreeFWait (Free _) (Pure _) = GT
      compareFreeFWait (Free wait1) (Free wait2) = compareWait wait1 wait2

      -- Separate pure from free values
      partitionFreeF ::
        [FreeF f a b] ->
        ([a], [f b])
      partitionFreeF [] = ([], [])
      partitionFreeF (Pure a : xs) = let (as, fbs) = partitionFreeF xs in (a : as, fbs)
      partitionFreeF (Free fb : xs) = let (as, fbs) = partitionFreeF xs in (as, fb : fbs)

      -- Shift a waiting action by some duration
      shift ::
        (TimeDifference diff) =>
        diff ->
        Wait diff a ->
        Wait diff a
      shift diff1 (Wait diff2 a) = Wait (diff2 `difference` diff1) a

      -- Shift a list of free actions by the duration of the head
      -- (assuming the list is sorted).
      -- If the head is pure, return it with the remaining actions,
      -- otherwise wait the minimum duration, give the continuation of the head,
      -- and shift the remaining actions by that minimum duration.
      shiftListOnce ::
        (TimeDifference diff) =>
        NonEmpty (FreeF (Wait diff) a b) ->
        Either
          (NonEmpty a, [Wait diff b]) -- Pure value has completed
          (Wait diff (b, [Wait diff b])) -- All values wait
      shiftListOnce actions = case partitionFreeF $ toList actions of
        (a : as, waits) -> Left (a :| as, waits)
        ([], Wait diff cont : waits) -> Right $ Wait diff (cont, shift diff <$> waits)
        ([], []) -> error "ScheduleT.shiftListOnce: Internal error. Please report as a bug: https://github.com/turion/monad-schedule/issues/new"

      -- Repeatedly shift the list by the smallest available waiting duration
      -- until some actions return as pure.
      -- Return its result, together with the remaining free actions.
      shiftList ::
        (TimeDifference diff, Ord diff, Monad m, MonadSchedule m) =>
        NonEmpty (FreeF (Wait diff) a (ScheduleT diff m a)) ->
        -- \^ Actionable
        [ScheduleT diff m a] ->
        -- \^ Delayed
        ScheduleT diff m (NonEmpty a, [ScheduleT diff m a])
      -- FIXME Don't I need to shift delayed as well?
      shiftList actions delayed = case shiftListOnce actions of
        -- Some actions returned. Wrap up the remaining ones.
        Left (as, waits) -> return (as, delayed ++ (FreeT . return . Free <$> waits))
        -- No action has returned.
        -- Wait the remaining time and start scheduling again.
        Right (Wait diff (cont, waits)) -> do
          wait diff
          let (zeroWaits, nonZeroWaits) = partition (isZero . getDiff) waits
              zeroWaitsUnwrapped = awaited <$> zeroWaits
          schedule (cont :| delayed ++ zeroWaitsUnwrapped ++ (FreeT . return . Free <$> nonZeroWaits))
