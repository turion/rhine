{- | An 'Automaton' with a CPS 'WriterT' layer outputs an extra monoid value on every step.

This module mirrors 'Data.Automaton.Trans.Writer' but uses the CPS-encoded
'Control.Monad.Trans.Writer.CPS', which avoids the space leak of the lazy
variant. The 'Monoid' constraint is required on every operation because
'CPS.writerT'/'CPS.runWriterT' thread the monoid through internal state,
unlike the strict variant which exposes the tuple directly.
-}
module Data.Automaton.Trans.Writer.CPS (
  module Control.Monad.Trans.Writer.CPS,
  writerS,
  runWriterS,
)
where

-- transformers
import Control.Monad.Trans.Writer.CPS hiding (liftCallCC, liftCatch, pass)

-- automaton
import Data.Automaton (Automaton, withAutomaton)
import Data.Stream.Result (Result (Result))

{- | Convert an extra log output into a 'WriterT' effect.

This is the opposite of 'runWriterS'.
-}
writerS ::
  (Functor m, Monad m, Monoid w) =>
  Automaton m a (w, b) ->
  Automaton (WriterT w m) a b
writerS = withAutomaton $ \f a -> writerT $ (\(Result s (w, b)) -> (Result s b, w)) <$> f a
{-# INLINE writerS #-}

{- | Convert a 'WriterT' effect into an extra log output.

This is the opposite of 'writerS'.
-}
runWriterS ::
  (Functor m, Monad m, Monoid w) =>
  Automaton (WriterT w m) a b ->
  Automaton m a (w, b)
runWriterS = withAutomaton $ \f a ->
  (\(Result s b, w) -> Result s (w, b))
    <$> runWriterT (f a)
{-# INLINE runWriterS #-}
