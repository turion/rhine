{- | Handle a global 'AccumT' layer in an 'Automaton'.

A global accumulation state can be hidden by an automaton by making it an internal state.
-}
module Data.Automaton.Trans.Accum (
  module Control.Monad.Trans.Accum,
  accumS,
  runAccumS,
  runAccumS_,
  runAccumS__,
)
where

-- base
import Control.Arrow (returnA)
import Data.Functor ((<&>))

-- transformers
import Control.Monad.Trans.Accum

-- automaton
import Data.Automaton (Automaton, feedback, withAutomaton)
import Data.Stream.Result (Result (..))

{- | Convert from explicit states to the 'AccumT' monad transformer.

The original automaton is interpreted to take the current accumulated state as input and return the log to be appended as output.

This is the opposite of 'runAccumS'.
-}
accumS :: (Functor m) => Automaton m (w, a) (w, b) -> Automaton (AccumT w m) a b
accumS = withAutomaton $ \f a -> AccumT $ \w ->
  (\(Result s (w', b)) -> (Result s b, w'))
    <$> f (w, a)

{- | Make the accumulation transition in 'AccumT' explicit as 'Automaton' inputs and outputs.

This is the opposite of 'accumS'.
-}
runAccumS :: (Functor m) => Automaton (AccumT w m) a b -> Automaton m (w, a) (w, b)
runAccumS = withAutomaton $ \f (w, a) ->
  (\(Result s b, w') -> Result s (w', b))
    <$> runAccumT (f a) w

{- | Convert global accumulation state to internal state of an 'Automaton'.

The current state is output on every step.
-}
runAccumS_ ::
  (Functor m, Monoid w, Monad m) =>
  -- | An automaton with a global accumulation state effect
  Automaton (AccumT w m) a b ->
  Automaton m a (w, b)
runAccumS_ automaton = feedback mempty $ proc (a, wState) -> do
  (wAdd, b) <- runAccumS automaton -< (wState, a)
  let wState' = wState <> wAdd
  returnA -< ((wState', b), wState')

-- | Like 'runAccumS_', but don't output the current accum.
runAccumS__ :: (Functor m, Monoid w, Monad m) => Automaton (AccumT w m) a b -> Automaton m a b
runAccumS__ automaton = runAccumS_ automaton <&> snd
