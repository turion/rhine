module Data.Stream.Except where

-- base
import Control.Monad (ap)
import Data.Void

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

-- mmorph
import Control.Monad.Morph (MFunctor, hoist)

-- selective
import Control.Selective

-- automaton
import Data.Stream.Final (Final (..))
import Data.Stream.Final.Except
import Data.Stream.Optimized (OptimizedStreamT, applyExcept, constM, selectExcept)
import Data.Stream.Optimized qualified as StreamOptimized

data StreamExcept m a e
  = -- | When using '>>=', this encoding needs to be used.
    FinalExcept (Final (ExceptT e m) a)
  | -- | This is usually the faster encoding, as it can be optimized by GHC.
    InitialExcept (OptimizedStreamT (ExceptT e m) a)

toFinal :: (Functor m) => StreamExcept m a e -> Final (ExceptT e m) a
toFinal (FinalExcept final) = final
toFinal (InitialExcept initial) = StreamOptimized.toFinal initial

runStreamExcept :: StreamExcept m a e -> OptimizedStreamT (ExceptT e m) a
runStreamExcept (FinalExcept final) = StreamOptimized.fromFinal final
runStreamExcept (InitialExcept initial) = initial

instance (Monad m) => Functor (StreamExcept m a) where
  fmap f (FinalExcept fe) = FinalExcept $ hoist (withExceptT f) fe
  fmap f (InitialExcept ae) = InitialExcept $ hoist (withExceptT f) ae

instance (Monad m) => Applicative (StreamExcept m a) where
  pure = InitialExcept . constM . throwE
  InitialExcept f <*> InitialExcept a = InitialExcept $ applyExcept f a
  f <*> a = ap f a

instance (Monad m) => Selective (StreamExcept m a) where
  select (InitialExcept e) (InitialExcept f) = InitialExcept $ selectExcept e f
  select e f = selectM e f

-- | 'return'/'pure' throw exceptions, '(>>=)' uses the last thrown exception as input for an exception handler.
instance (Monad m) => Monad (StreamExcept m a) where
  (>>) = (*>)
  ae >>= f = FinalExcept $ handleExceptT (toFinal ae) (toFinal . f)

safely :: (Monad m) => StreamExcept m a Void -> OptimizedStreamT m a
safely = hoist (fmap (either absurd id) . runExceptT) . runStreamExcept
