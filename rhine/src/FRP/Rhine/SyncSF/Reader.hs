{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}
module FRP.Rhine.SyncSF.Reader where

-- base
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.Reader

-- dunai
import qualified Control.Monad.Trans.MSF.Reader as MSF

-- rhine
import FRP.Rhine.SyncSF.Core


-- | Commute two 'ReaderT' transformer layers past each other
commuteReaders :: ReaderT r1 (ReaderT r2 m) a -> ReaderT r2 (ReaderT r1 m) a
commuteReaders a
  = ReaderT $ \r1 -> ReaderT $ \r2 -> runReaderT (runReaderT a r2) r1

-- | Create ("wrap") a 'ReaderT' layer in the monad stack of a behaviour.
--   Each tick, the 'ReaderT' side effect is performed
--   by passing the original behaviour the extra @r@ input.
readerS
  :: Monad m
  => SyncSF m cl (a, r) b -> SyncSF (ReaderT r m) cl a b
readerS behaviour
  = liftMSFPurer commuteReaders $ MSF.readerS $ arr swap >>> behaviour

-- | Remove ("run") a 'ReaderT' layer from the monad stack
--   by making it an explicit input to the behaviour.
runReaderS
  :: Monad m
  => SyncSF (ReaderT r m) cl a b -> SyncSF m cl (a, r) b
runReaderS behaviour
  = arr swap >>> (MSF.runReaderS $ liftMSFPurer commuteReaders behaviour)

-- | Remove a 'ReaderT' layer by passing the readonly environment explicitly.
runReaderS_
  :: Monad m
  => SyncSF (ReaderT r m) cl a b -> r -> SyncSF m cl a b
runReaderS_ behaviour r = arr (, r) >>> runReaderS behaviour
