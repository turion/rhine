{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
This module supplies a general purpose monad transformer
that adds a syntactical "delay", or "waiting" side effect.

This allows for universal and deterministic scheduling of clocks
that implement their waiting actions in 'ScheduleT'.
See 'FRP.Rhine.Schedule.Trans' for more details.
-}

{-# LANGUAGE DeriveFunctor #-}
module Control.Monad.Schedule where


-- base
import Control.Concurrent
import GHC.TypeLits
import Prelude hiding (zip, head, tail)

-- transformers
import Control.Monad.IO.Class

-- free
import Control.Monad.Trans.Free
import Data.Function ((&))

-- vector-sized
import Data.Vector.Sized (empty, cons, fromTuple, tail, head, fromList, toSized, toList, zip, Vector)
import Data.Maybe (fromJust)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Either (fromLeft, fromRight)
import Control.Monad.Trans.Reader (runReaderT, ReaderT (ReaderT), mapReaderT)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.State.Strict (runStateT, StateT (StateT), mapStateT)

-- TODO Implement Time via StateT

-- * The 'ScheduleT' monad transformer

{- |
A functor implementing a syntactical "waiting" action.

* 'diff' represents the duration to wait.
* 'a' is the encapsulated value.
-}
data Wait diff a = Wait diff a
  deriving Functor

{- |
Values in @ScheduleT diff m@ are delayed computations with side effects in 'm'.
Delays can occur between any two side effects, with lengths specified by a 'diff' value.
These delays don't have any semantics, it can be given to them with 'runScheduleT'.
-}
type ScheduleT diff = FreeT (Wait diff)

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

-- * The 'MonadSchedule' class

class Monad m => MonadWait m where
  type Diff m
  -- | Wait for the given time.
  wait :: Diff m -> m ()

instance Monad m => MonadWait (ScheduleT diff m) where
  type Diff (ScheduleT diff m) = diff
  wait diff = FreeT $ return $ Free $ Wait diff $ return ()

{- | A monad in which we can reason about and edit the planned delay times.

An action @ma :: m a@ represents a potentially blocking computation.
We can always simply execute the action to retrieve the value,
but that may block the whole thread.
Instead, we can 'peek' to find out for how long the value would be block at least,
or whether it is maybe already available.
If we find out that we do in fact have a good way to spend that time differently,
we can 'skip' an interval and try to see whether we're any closer to calculating the value.
-}
class (Ord (Diff m), Num (Diff m), MonadWait m) => MonadSchedule m where
  {- | Tell us how much time we will have to wait at least,
  if we would 'wait'.

  * 'peek' should not introduce delays itself, it should return immediately.
  * If 'peek ma' returns '(Just 0, ma')', then there may be further waiting time in 'ma''.
    However if it returns '(Nothing, ma')',
    then a call of 'wait ma'' should return immediately.
  -}
  peek :: m a -> m (Maybe (Diff m), m a)

  {- | Reduce the planned waiting time by the given interval.

  If @'peek' ma@ return @'Just' diff1@, and @diff1 > diff2@,
  then @'peek' $ 'skip' diff2 ma@ should return @'Just' $ diff1 - diff2@.
  But this is sometimes not reliably possible if 'peek' uses 'IO'.
  In such monads where the waiting time does not solely rely on algebraic structure
  (such as 'ScheduleT'), 'skip' may be implemented as best effort, or even as a noop.
  -}
  skip :: Diff m -> m a -> m a

instance (Ord diff, Num diff, Monad m) => MonadSchedule (ScheduleT diff m) where
  peek (FreeT ma) = FreeT $ do
    aWait <- ma
    case aWait of
      Pure a -> return $ Pure (Nothing, return a)
      Free (Wait diff cont) -> return $ Pure (Just diff, cont)

  skip diffSkip (FreeT ma) = FreeT $ do
    aWait <- ma
    case aWait of
      Pure a -> return $ Pure a
      Free (Wait diff cont) -> if diffSkip <= diff
        then return $ Free $ Wait (diff - diffSkip) cont
        else runFreeT $ skip (diffSkip - diff) cont

-- fixme maybe this works with overlapping instances
{-
instance (MonadTrans t, MonadWait m) => MonadWait (t m) where
  type Diff (t m) = Diff m
  wait = lift . wait
-}

instance MonadWait m => MonadWait (ReaderT r m) where
  type Diff (ReaderT r m) = Diff m
  wait = lift . wait

instance MonadSchedule m => MonadSchedule (ReaderT r m) where
  peek action = ReaderT $ \r -> do
    (diffMaybe, continuation) <- peek $ runReaderT action r
    return (diffMaybe, lift continuation)
  skip = mapReaderT . skip

instance MonadWait m => MonadWait (StateT r m) where
  type Diff (StateT r m) = Diff m
  wait = lift . wait

-- Is that right? Or should peek return a continuation?
-- | Does not advance the state when 'peek'ing.
instance MonadSchedule m => MonadSchedule (StateT s m) where
  peek action = StateT $ \s -> do
    (diffMaybe, continuation) <- peek $ runStateT action s
    return ((diffMaybe, _), _)

  skip = mapStateT . skip

-- | Run the given actions simultaneously.
--   The first action that returns a value now is executed, and the value returned.
--   No guarantee for scheduling is made.
--   Internally, those actions that are possibly due earliest are preferred.
raceVector
  :: (KnownNat (1 + n), MonadSchedule m)
  => Vector (1 + n) (m a)
  -> m (a, Vector n (m a))
raceVector actions = do
  -- Find out, for each action, when it will yield next
  peeks <- sequence $ peek <$> actions
  -- Sort such that the head is the action that will yield first
  let sortedPeeks = sortVectorWith fst peeks
  case head sortedPeeks of
    -- The action is ready. Return its value.
    (Nothing, ma) -> do
      a <- ma
      return (a, tail $ snd <$> sortedPeeks)
    -- Even the next action needs some time.
    (Just diff, _) -> do
      -- Wait the minimum necessary time
      wait diff
      -- All the actions don't need to do that waiting anymore.
      raceVector $ skip diff <$> actions
  where
    sortVectorWith :: (Ord b, KnownNat n) => (a -> b) -> Vector n a -> Vector n a
    sortVectorWith f = fromJust . fromList . (sortBy $ comparing f) . toList

-- | Runs two values in 'ScheduleT' concurrently
--   and returns the first one that yields a value
--   (defaulting to the first argument),
--   and a continuation for the other value.
race
  :: (MonadSchedule m)
  =>            m a -> m b
  -> m (Either
               (  a,   m b)
               (m a,     b)
     )
race ma mb = do
  let actions = cons (Left <$> ma) $ cons (Right <$> mb) empty
  result <- raceVector actions
  case result of
    (Left  a, vector) -> return $ Left  (a, fromRight impossible <$> head vector)
    (Right b, vector) -> return $ Right (fromLeft impossible <$> head vector, b)
  where
    impossible = error "MonadSchedule.race"

-- | Runs both schedules concurrently and returns their results at the end.
async
  :: MonadSchedule m
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

raceMSF
  :: MonadSchedule m
  => MSF (ListT m)
