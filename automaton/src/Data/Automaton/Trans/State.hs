{- | Handle a global 'StateT' layer in an 'Automaton'.

A global state can be hidden by an automaton by making it an internal state.

This module is based on the _strict_ state monad 'Control.Monad.Trans.State.Strict',
so when combining it with other modules such as @mtl@'s,
the strict version has to be included, i.e. 'Control.Monad.State.Strict'
instead of 'Control.Monad.State' or 'Control.Monad.State.Lazy'.
-}
module Data.Automaton.Trans.State (
  module Control.Monad.Trans.State.Strict,
  stateS,
  runStateS,
  runStateS_,
  runStateS__,
)
where

-- base
import Control.Arrow (arr, (>>>))
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.State.Strict

-- automaton
import Data.Automaton (Automaton, feedback, withAutomaton)
import Data.Stream.Result (Result (..))

-- * 'State' 'Automaton' running and wrapping

{- | Convert from explicit states to the 'StateT' monad transformer.

The original automaton is interpreted to take a state as input and return the updated state as output.

This is the opposite of 'runStateS'.
-}
stateS :: (Functor m, Monad m) => Automaton m (s, a) (s, b) -> Automaton (StateT s m) a b
stateS = withAutomaton $ \f a -> StateT $ \s ->
  (\(Result s' (s, b)) -> (Result s' b, s))
    <$> f (s, a)

{- | Make the state transition in 'StateT' explicit as 'Automaton' inputs and outputs.

This is the opposite of 'stateS'.
-}
runStateS :: (Functor m, Monad m) => Automaton (StateT s m) a b -> Automaton m (s, a) (s, b)
runStateS = withAutomaton $ \f (s, a) ->
  (\(Result s' b, s) -> Result s' (s, b))
    <$> runStateT (f a) s

{- | Convert global state to internal state of an 'Automaton'.

The current state is output on every step.
-}
runStateS_ ::
  (Functor m, Monad m) =>
  -- | An automaton with a global state effect
  Automaton (StateT s m) a b ->
  -- | The initial global state
  s ->
  Automaton m a (s, b)
runStateS_ automaton s =
  feedback s $
    arr swap >>> runStateS automaton >>> arr (\(s', b) -> ((s', b), s'))

-- | Like 'runStateS_', but don't output the current state.
runStateS__ :: (Functor m, Monad m) => Automaton (StateT s m) a b -> s -> Automaton m a b
runStateS__ automaton s = runStateS_ automaton s >>> arr snd
