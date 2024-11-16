{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Create and remove 'StateT' layers in 'ClSF's.
-}
module FRP.Rhine.ClSF.State where

-- base
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

-- automaton
import Data.Automaton.Trans.State qualified as Automaton

-- rhine
import FRP.Rhine.ClSF.Core

commuteState :: ReaderT r (StateT s m) a -> StateT s (ReaderT r m) a
commuteState a =
  StateT $ \s -> ReaderT $ \r -> runStateT (runReaderT a r) s
{-# INLINE commuteState #-}

commuteStateBack :: StateT s (ReaderT r m) a -> ReaderT r (StateT s m) a
commuteStateBack a =
  ReaderT $ \r -> StateT $ \s -> runReaderT (runStateT a s) r
{-# INLINE commuteStateBack #-}

stateS ::
  (Monad m) =>
  ClSF m cl (s, a) (s, b) ->
  ClSF (StateT s m) cl a b
stateS behaviour = hoistS commuteStateBack $ Automaton.stateS behaviour
{-# INLINE stateS #-}

runStateS ::
  (Monad m) =>
  ClSF (StateT s m) cl a b ->
  ClSF m cl (s, a) (s, b)
runStateS behaviour = Automaton.runStateS (hoistS commuteState behaviour)
{-# INLINE runStateS #-}

runStateS_ ::
  (Monad m) =>
  ClSF (StateT s m) cl a b ->
  s ->
  ClSF m cl a (s, b)
runStateS_ behaviour s = Automaton.runStateS_ (hoistS commuteState behaviour) s
{-# INLINE runStateS_ #-}
