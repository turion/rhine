{- | An 'Automaton' with a 'ReaderT' layer has an extra input.

This module converts between explicit automata inputs and implicit 'ReaderT' inputs.
-}
module Data.Automaton.Trans.Reader (
  module Control.Monad.Trans.Reader,
  readerS,
  runReaderS,
  runReaderS_,
)
where

-- base
import Control.Arrow (arr, (>>>))

-- transformers
import Control.Monad.Trans.Reader

-- automaton
import Data.Automaton (Automaton, withAutomaton)

-- * Reader 'Automaton' running and wrapping

{- | Convert an explicit 'Automaton' input into an environment in the 'ReaderT' monad transformer.

This is the opposite of 'runReaderS'.
-}
readerS :: (Monad m) => Automaton m (r, a) b -> Automaton (ReaderT r m) a b
readerS = withAutomaton $ \f a -> ReaderT $ \r -> f (r, a)
{-# INLINE readerS #-}

{- | Convert an implicit 'ReaderT' environment into an explicit 'Automaton' input.

This is the opposite of 'readerS'.
-}
runReaderS :: (Monad m) => Automaton (ReaderT r m) a b -> Automaton m (r, a) b
runReaderS = withAutomaton $ \f (r, a) -> runReaderT (f a) r
{-# INLINE runReaderS #-}

-- | Eliminate a 'ReaderT' layer by providing its environment statically.
runReaderS_ :: (Monad m) => Automaton (ReaderT s m) a b -> s -> Automaton m a b
runReaderS_ automaton s = arr (s,) >>> runReaderS automaton
{-# INLINE runReaderS_ #-}
