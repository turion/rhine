{-# LANGUAGE LambdaCase #-}

module Data.Automaton.Except where

-- base
import Control.Monad (ap)

-- transformers
import Control.Monad.Trans.Except

-- mmorph
import Control.Monad.Morph (hoist)

-- rhine

import Data.Automaton.Final (Final (..))
import Data.Automaton.Final.Except
import Data.Automaton.Optimized (OptimizedAutomatonT, applyExcept, constM)
import Data.Automaton.Optimized qualified as AutomatonOptimized
import Data.Void

data AutomatonExcept m a e
  = -- | When using '>>=', this encoding needs to be used.
    FinalExcept (Final (ExceptT e m) a)
  | -- | This is usually the faster encoding, as it can be optimized by GHC.
    InitialExcept (OptimizedAutomatonT (ExceptT e m) a)

toFinal :: (Functor m) => AutomatonExcept m a e -> Final (ExceptT e m) a
toFinal (FinalExcept final) = final
toFinal (InitialExcept initial) = AutomatonOptimized.toFinal initial

runExceptS :: AutomatonExcept m a e -> OptimizedAutomatonT (ExceptT e m) a
runExceptS (FinalExcept final) = AutomatonOptimized.fromFinal final
runExceptS (InitialExcept initial) = initial

instance (Monad m) => Functor (AutomatonExcept m a) where
  fmap f (FinalExcept fe) = FinalExcept $ hoist (withExceptT f) fe
  fmap f (InitialExcept ae) = InitialExcept $ hoist (withExceptT f) ae

instance (Monad m) => Applicative (AutomatonExcept m a) where
  pure = InitialExcept . constM . throwE
  InitialExcept f <*> InitialExcept a = InitialExcept $ applyExcept f a
  f <*> a = ap f a

-- | 'return'/'pure' throw exceptions, '(>>=)' uses the last thrown exception as input for an exception handler.
instance (Monad m) => Monad (AutomatonExcept m a) where
  (>>) = (*>)
  ae >>= f = FinalExcept $ handleExceptT (toFinal ae) (toFinal . f)

safely :: (Monad m) => AutomatonExcept m a Void -> OptimizedAutomatonT m a
safely = hoist (fmap (either absurd id) . runExceptT) . runExceptS
