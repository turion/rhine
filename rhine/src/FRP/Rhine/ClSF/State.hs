{- |
Create and remove 'StateT' layers in 'ClSF's.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.ClSF.State where

-- base
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

-- dunai
import qualified Control.Monad.Trans.MSF.State as MSF

-- rhine
import FRP.Rhine.ClSF.Core


-- | Commute 'ReaderT' transformer layer past 'StateT'
commuteReaderState :: StateT s (ReaderT r m) a -> ReaderT r (StateT s m) a
commuteReaderState a
  = ReaderT $ \r -> StateT $ \s -> runReaderT (runStateT a s) r

-- | Commute 'StateT' transformer layer past 'ReaderT'
commuteStateReader :: ReaderT r (StateT s m) a -> StateT s (ReaderT r m) a
commuteStateReader a
  = StateT $ \s -> ReaderT $ \r -> runStateT (runReaderT a r) s

-- | Create ("wrap") a 'StateT' layer in the monad stack of a behaviour.
--   Each tick, the 'StateT' side effect is performed
--   by passing the original behaviour the extra @s@ input.
stateS
  :: Monad m
  => ClSF m cl (s, a) (s, b) -> ClSF (StateT s m) cl a b
stateS behaviour
  = morphS commuteReaderState $ arr MSF.stateS behaviour

-- | Remove ("run") a 'StateT' layer from the monad stack
--   by making it an explicit input to the behaviour.
runStateS
  :: Monad m
  => ClSF (StateT s m) cl a b -> ClSF m cl (s, a) (s, b)
runStateS behaviour
  = MSF.runStateS $ morphS commuteStateReader behaviour

-- | Build an 'ClSF' /function/ that takes a fixed state as additional input,
-- from an 'ClSF' in the 'State' monad, and outputs the new state with every
-- transformation step.
runStateS_
  :: Monad m
  => ClSF (StateT s m) cl a b -> s -> ClSF m cl a (s, b)
runStateS_ behaviour s =
  feedback s $
    arr swap >>> runStateS behaviour >>> arr (\(s', b) -> ((s', b), s'))

-- | Build an 'ClSF' /function/ that takes a fixed state as additional input,
-- from an 'ClSF' in the 'State' monad.
runStateS__
  :: Monad m
  => ClSF (StateT s m) cl a b -> s -> ClSF m cl a b
runStateS__ behaviour s = runStateS_ behaviour s >>> arr snd
