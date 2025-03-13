{- | Handle a global 'ChangesetT' layer in an 'Automaton'.

A global accumulation state can be hidden by an automaton by making it an internal state.
-}
module Data.Automaton.Trans.Changeset (
  module Control.Monad.Trans.Changeset,
  changesetS,
  getChangesetS,
  runChangesetS,
  runChangesetS_,
)
where

-- base
import Control.Arrow (arr, returnA, (>>>))
import Data.Functor ((<&>))

-- changeset
import Control.Monad.Trans.Changeset
import Data.Monoid.RightAction (RightAction (actRight))

-- automaton
import Data.Automaton (Automaton, feedback, withAutomaton)
import Data.Stream.Result (Result (..))

{- | Convert from explicit states to the 'ChangesetT' monad transformer.

The original automaton is interpreted to take the current accumulated state as input and return the log to be appended as output.

This is the opposite of 'runChangesetS'.
-}
changesetS :: (Functor m) => Automaton m (s, a) (w, b) -> Automaton (ChangesetT s w m) a b
changesetS = withAutomaton $ \f a -> ChangesetT $ \s ->
  f (s, a)
    <&> (\(Result s' (w, b)) -> (w, Result s' b))

{- | Make the accumulation transition in 'ChangesetT' explicit as 'Automaton' inputs and outputs.

This is the opposite of 'changesetS'.
-}
getChangesetS :: (Functor m) => Automaton (ChangesetT s w m) a b -> Automaton m (s, a) (w, b)
getChangesetS = withAutomaton $ \f (s, a) ->
  getChangesetT (f a) s
    <&> (\(w, Result s' b) -> Result s' (w, b))

{- | Convert global accumulation state to internal state of an 'Automaton'.

The current state is output on every step.
-}
runChangesetS ::
  (Monad m, Monoid w, RightAction w s) =>
  -- | Initial state
  s ->
  -- | An automaton with a global accumulation state effect
  Automaton (ChangesetT s w m) a b ->
  Automaton m a (s, b)
runChangesetS s automaton = feedback s $ proc (a, s) -> do
  (w, b) <- getChangesetS automaton -< (s, a)
  let s' = s `actRight` w
  returnA -< ((s', b), s')

-- | Like 'runChangesetS', but don't output the current state.
runChangesetS_ :: (Monoid w, Monad m, RightAction w s) => s -> Automaton (ChangesetT s w m) a b -> Automaton m a b
runChangesetS_ s automaton = runChangesetS s automaton >>> arr snd
