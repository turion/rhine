{- |
This module supplies a general-purpose monad transformer that adds a syntactical
"delay", or "waiting" side effect, used for time-based scheduling of automata.

'ScheduleT' is used as the monad for automata that need to express timing
information: an automaton in @'ScheduleT' diff m@ can @'wait' diff@ to signal
how long until its next output.

The 'Data.Automaton.Schedule.MonadSchedule' instance for 'ScheduleT' interleaves
several such automata in time order.

This module re-exports the underlying 'WriterT' representation (CPS-encoded).
Each call to 'wait' contributes to the writer log; the total wait per
'ScheduleT' action is the monoidal sum of all interspersed 'wait's. This
means consecutive 'wait's fuse — a consumer cannot distinguish
@wait 3 >> wait 2@ from @wait 5@. For typical clock implementations
(one 'wait' per logical tick) this is irrelevant.
-}
module Data.Automaton.Schedule.Trans (module Data.Automaton.Schedule.Trans) where

-- base
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Identity (Identity (..))

-- transformers
import Control.Monad.Trans.Class (MonadTrans (..))

-- automaton
import Data.Automaton (Automaton, hoistS)
import Data.Automaton.Trans.Writer.CPS qualified as W

-- * 'ScheduleT'

{- |
Values in @ScheduleT diff m@ are delayed computations with side effects in @m@.
Delays can occur between any two side effects, with lengths specified by a @diff@
value. Consecutive delays fuse monoidally: the total wait is @<>@ of all delays.

These delays don't have any semantics on their own; semantics can be given with
'runScheduleT'.

The 'Data.Automaton.Schedule.MonadSchedule' instance for 'ScheduleT' interprets
delays as logical time and interleaves several such computations in order of
their next scheduled time.
-}
newtype ScheduleT diff m a = ScheduleT
  { getScheduleT :: W.WriterT diff m a
  -- ^ Unwrap 'ScheduleT' to the underlying CPS 'WriterT'.
  }
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | 'ScheduleT' over the 'Identity' monad.
type Schedule diff = ScheduleT diff Identity

-- | The side effect that waits for a specified amount.
wait :: (Monoid diff, Monad m) => diff -> ScheduleT diff m ()
wait = ScheduleT . W.tell
{-# INLINE wait #-}

{- | Supply a semantic meaning to the accumulated wait.

For every 'ScheduleT diff m a' the @waitAction@ is executed once, with the
total accumulated @diff@ value. This is observationally equivalent to
running the action once per individual 'wait' boundary whenever the
underlying effect is purely additive (e.g. 'threadDelay').
-}
runScheduleT :: (Monoid diff, Monad m) => (diff -> m ()) -> ScheduleT diff m a -> m a
runScheduleT waitAction (ScheduleT w) = do
  (a, total) <- W.runWriterT w
  waitAction total
  pure a
{-# INLINE runScheduleT #-}

{- | Run a 'ScheduleT' value, ignoring the waiting actions.

Usually, you would apply this function after having scheduled several automata together with 'schedule',
and you want to get the final result of the schedule without caring about the timing.
-}
evalScheduleT :: (Monoid diff, Monad m) => ScheduleT diff m a -> m a
evalScheduleT (ScheduleT w) = fst <$> W.runWriterT w
{-# INLINE evalScheduleT #-}

-- | Run a 'Schedule' value, ignoring the waiting actions.
evalSchedule :: (Monoid diff) => Schedule diff a -> a
evalSchedule = runIdentity . evalScheduleT
{-# INLINE evalSchedule #-}

{- | Run a 'ScheduleT' value in a 'MonadIO',
interpreting the times as milliseconds.
-}
runScheduleIO ::
  (MonadIO m, Integral n, Monoid n) =>
  ScheduleT n m a ->
  m a
runScheduleIO = runScheduleT waitms
{-# INLINE runScheduleIO #-}

-- | Wait for the given number of milliseconds.
waitms :: (MonadIO m, Integral n) => n -> m ()
waitms = liftIO . threadDelay . (* 1000) . fromIntegral
{-# INLINE waitms #-}

{- | Break down the steps of an 'Automaton' in 'ScheduleT' into a paired
wait duration and output.

Each tick produces @(diff, b)@: the total wait the underlying action would
have consumed before yielding @b@, and the output itself.  The dual of
'scheduleS'.
-}
runScheduleS :: (Monoid diff, Functor m, Monad m) => Automaton (ScheduleT diff m) a b -> Automaton m a (diff, b)
runScheduleS = W.runWriterS . hoistS getScheduleT
{-# INLINE runScheduleS #-}

{- | Embed an automaton that produces @(diff, b)@ values into
'ScheduleT', interpreting the @diff@ component as a 'wait' duration.

The dual of 'runScheduleS': whenever the inner automaton emits @(diff, b)@,
'scheduleS' calls @'wait' diff@ and then yields @b@.
-}
scheduleS :: (Monoid diff, Monad m) => Automaton m a (diff, b) -> Automaton (ScheduleT diff m) a b
scheduleS = hoistS ScheduleT . W.writerS
{-# INLINE scheduleS #-}
