{- | An 'Automaton' with a 'WriterT' layer outputs an extra monoid value on every step.

It is based on the _strict_ writer monad 'Control.Monad.Trans.Writer.Strict',
so when combining it with other modules such as @mtl@'s,
the strict version has to be included, i.e. 'Control.Monad.Writer.Strict'
instead of 'Control.Monad.Writer' or 'Control.Monad.Writer.Lazy'.
-}
module Data.Automaton.Trans.Writer (
  module Control.Monad.Trans.Writer.Strict,
  writerS,
  runWriterS,
)
where

-- transformers
import Control.Monad.Trans.Writer.Strict hiding (liftCallCC, liftCatch, pass)

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
writerS = withAutomaton $ \f a -> WriterT $ (\(Result s (w, b)) -> (Result s b, w)) <$> f a

{- | Convert a 'WriterT' effect into an extra log output.

This is the opposite of 'writerS'.
-}
runWriterS ::
  (Functor m, Monad m) =>
  Automaton (WriterT w m) a b ->
  Automaton m a (w, b)
runWriterS = withAutomaton $ \f a ->
  (\(Result s b, w) -> Result s (w, b))
    <$> runWriterT (f a)
