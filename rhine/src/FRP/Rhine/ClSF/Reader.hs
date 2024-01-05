{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Create and remove 'ReaderT' layers in 'ClSF's.
-}
module FRP.Rhine.ClSF.Reader where

-- base
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.Reader

-- automaton
import Data.Automaton.Trans.Reader qualified as Automaton

-- rhine
import FRP.Rhine.ClSF.Core

-- | Commute two 'ReaderT' transformer layers past each other
commuteReaders :: ReaderT r1 (ReaderT r2 m) a -> ReaderT r2 (ReaderT r1 m) a
commuteReaders a =
  ReaderT $ \r1 -> ReaderT $ \r2 -> runReaderT (runReaderT a r2) r1
{-# INLINE commuteReaders #-}

{- | Create ("wrap") a 'ReaderT' layer in the monad stack of a behaviour.
   Each tick, the 'ReaderT' side effect is performed
   by passing the original behaviour the extra @r@ input.
-}
readerS ::
  (Monad m) =>
  ClSF m cl (a, r) b ->
  ClSF (ReaderT r m) cl a b
readerS behaviour =
  hoistS commuteReaders $ Automaton.readerS $ arr swap >>> behaviour
{-# INLINE readerS #-}

{- | Remove ("run") a 'ReaderT' layer from the monad stack
   by making it an explicit input to the behaviour.
-}
runReaderS ::
  (Monad m) =>
  ClSF (ReaderT r m) cl a b ->
  ClSF m cl (a, r) b
runReaderS behaviour =
  arr swap >>> Automaton.runReaderS (hoistS commuteReaders behaviour)
{-# INLINE runReaderS #-}

-- | Remove a 'ReaderT' layer by passing the readonly environment explicitly.
runReaderS_ ::
  (Monad m) =>
  ClSF (ReaderT r m) cl a b ->
  r ->
  ClSF m cl a b
runReaderS_ behaviour r = arr (,r) >>> runReaderS behaviour
{-# INLINE runReaderS_ #-}
