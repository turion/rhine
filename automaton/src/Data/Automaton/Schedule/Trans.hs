{-# LANGUAGE DeriveFunctor #-}

{- |
This module supplies a general-purpose monad transformer that adds a syntactical
"delay", or "waiting" side effect, used for time-based scheduling of automata.

'ScheduleT' is used as the monad for automata that need to express timing
information: an automaton in @'ScheduleT' diff m@ can @'wait' diff@ to signal
how long until its next output.

The 'Data.Automaton.Schedule.MonadSchedule' instance for 'ScheduleT' interleaves
several such automata in time order.
-}
module Data.Automaton.Schedule.Trans (module Data.Automaton.Schedule.Trans) where

-- base
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Classes (Eq1 (..), liftEq)
import Data.Functor.Identity (Identity (..))
import Data.Ord (comparing)

-- transformers
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..))

-- free
import Control.Monad.Trans.Free (FreeF (..), FreeT (..), iterT, liftF, runFreeT)

-- automaton
import Data.Automaton (Automaton, handleAutomaton)
import Data.Stream (StreamT (..))
import Data.Stream.Result (Result (..))

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

Note that this would not give a lawful 'Ord' instance since we do not compare
the @a@.
-}
compareWait :: (Ord diff) => Wait diff a -> Wait diff a -> Ordering
compareWait = comparing getDiff

-- * 'ScheduleT'

{- |
Values in @ScheduleT diff m@ are delayed computations with side effects in @m@.
Delays can occur between any two side effects, with lengths specified by a @diff@
value.

These delays don't have any semantics on their own; semantics can be given with
'runScheduleT'.

The 'Data.Automaton.Schedule.MonadSchedule' instance for 'ScheduleT' interprets
delays as logical time and interleaves several such computations in order of
their next scheduled time.
-}
type ScheduleT diff = FreeT (Wait diff)

-- | 'ScheduleT' over the 'Identity' monad.
type Schedule diff = ScheduleT diff Identity

-- | The side effect that waits for a specified amount.
wait :: (Monad m) => diff -> ScheduleT diff m ()
wait diff = FreeT $ pure $ Free $ Wait diff $ pure ()

{- | Supply a semantic meaning to 'Wait'.
For every occurrence of @Wait diff@ in the @ScheduleT diff m a@ value,
a waiting action is executed, depending on @diff@.
-}
runScheduleT :: (Monad m) => (diff -> m ()) -> ScheduleT diff m a -> m a
runScheduleT waitAction = iterT $ \(Wait n ma) -> waitAction n >> ma

{- | Run a 'ScheduleT' value, ignoring the waiting actions.

Usually, you would apply this function after having scheduled several automata together with 'schedule',
and you want to get the final result of the schedule without caring about the timing.
-}
evalScheduleT :: (Monad m) => ScheduleT diff m a -> m a
evalScheduleT = runScheduleT $ const $ pure ()

-- | Run a 'Schedule' value, ignoring the waiting actions.
evalSchedule :: Schedule diff a -> a
evalSchedule = runIdentity . evalScheduleT

{- | Run a 'ScheduleT' value in a 'MonadIO',
interpreting the times as milliseconds.
-}
runScheduleIO ::
  (MonadIO m, Integral n) =>
  ScheduleT n m a ->
  m a
runScheduleIO = runScheduleT waitms

-- | Wait for the given number of milliseconds.
waitms :: (MonadIO m, Integral n) => n -> m ()
waitms = liftIO . threadDelay . (* 1000) . fromIntegral

{- | Formally execute all waiting actions,
returning the final value and all moments when the schedule would have waited.
-}
execScheduleT :: (Monad m) => ScheduleT diff m a -> m (a, [diff])
execScheduleT action = do
  free <- runFreeT action
  case free of
    Pure a -> pure (a, [])
    Free (Wait diff cont) -> do
      (a, diffs) <- execScheduleT cont
      pure (a, diff : diffs)

{- | Break down the steps of an 'Automaton' in 'ScheduleT' into waiting
effects and returning values.

Each tick either produces a @'Right' b@ (a regular output) or a
@'Left' diff@ (a wait duration), exposing the internal scheduling information
to the caller.  The dual of 'scheduleS'.
-}
runScheduleS :: (Functor m, Monad m) => Automaton (ScheduleT diff m) a b -> Automaton m a (Either diff b)
runScheduleS = handleAutomaton $ \StreamT {state, step} ->
  StreamT
    { state = step state
    , step = \s -> ReaderT $ \a -> do
        oneStep <- runFreeT $ runReaderT s a
        pure $ case oneStep of
          Pure (Result s' b) -> Result (step s') (Right b)
          Free (Wait diff cont) -> Result (lift cont) (Left diff)
    }
{-# INLINE runScheduleS #-}

{- | Embed an automaton that produces @'Either' diff b@ values into
'ScheduleT', interpreting @'Left' diff@ as a 'wait' instruction.

The dual of 'runScheduleS': whenever the inner automaton emits @'Left' diff@,
'scheduleS' calls @'wait' diff@ and then re-runs the step with the same
input until a @'Right' b@ is produced.
-}
scheduleS :: (Monad m) => Automaton m a (Either diff b) -> Automaton (ScheduleT diff m) a b
scheduleS = handleAutomaton $ \StreamT {state, step} ->
  let step' s = ReaderT $ \a -> do
        Result s' eitherDiffB <- lift $ runReaderT (step s) a
        case eitherDiffB of
          Right b -> pure $ Result s' b
          Left diff -> do
            wait diff
            runReaderT (step' s') a
   in StreamT
        { state
        , step = step'
        }
{-# INLINE scheduleS #-}

-- * The symbolic effect of skipping one step of an automaton

{- | A monad transformer that adds the ability to __skip__ one output step.

An automaton in @'SkipT' m@ may call 'skip' to signal that it wants to
defer its output to the next step.  'MonadSchedule' for 'SkipT' uses this to
implement cooperative, non-preemptive round-robin scheduling: an automaton
that has not yet skipped enough times simply skips instead of emitting an
output, giving other automata a chance to catch up.

See also 'runSkipS' and 'skip'.
-}
newtype SkipT m a = SkipT
  { getSkipT :: FreeT Identity m a
  -- ^ Unwrap 'SkipT' to the underlying free-monad transformer.
  }
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | 'SkipT' specialised to 'Identity': a pure automaton with skip steps.
type Yield = SkipT Identity

{- | Run an 'Automaton' in 'SkipT', exposing skipped steps as 'Nothing'.

Each tick of the result automaton corresponds to one tick of the input
automaton.  If the input automaton called 'skip' on that tick the result is
'Nothing'; otherwise it is 'Just' the output.
-}
runSkipS :: (Functor m, Monad m) => Automaton (SkipT m) a b -> Automaton m a (Maybe b)
runSkipS = handleAutomaton $ \StreamT {state, step} ->
  StreamT
    { state = step state
    , step = \s -> ReaderT $ \a -> do
        oneTick <- runFreeT $ getSkipT $ runReaderT s a
        pure $ case oneTick of
          Pure (Result s' b) -> Result (step s') (Just b)
          Free (Identity cont) -> Result (lift $ SkipT cont) Nothing
    }
{-# INLINE runSkipS #-}

-- | Signal that the current step should be skipped, deferring output to the next tick.
skip :: (Monad m) => SkipT m ()
skip = SkipT $ liftF $ pure ()

-- | Run a 'SkipT' action, discarding all 'skip' steps.
runSkipT :: (Monad m) => SkipT m a -> m a
runSkipT = iterT runIdentity . getSkipT

-- | Run a 'SkipT' action, executing @action@ for every 'skip' step.
runSkipTWith :: (Monad m) => m () -> SkipT m a -> m a
runSkipTWith action = iterT (\ima -> action >> runIdentity ima) . getSkipT

-- | Run a pure 'Yield' computation, discarding all skipped steps.
runYield :: Yield a -> a
runYield = runIdentity . runSkipT
