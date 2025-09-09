{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Schedule.OSThreadPool where

-- base
import Control.Concurrent
import Control.Monad (forM, replicateM, void)
import Control.Monad.IO.Class
import Data.Either (partitionEithers)
import Data.List.NonEmpty hiding (cycle, zip)
import Data.Proxy
import GHC.TypeLits
import Prelude hiding (take)

-- stm
import Control.Concurrent.STM

-- monad-schedule
import Control.Monad.Schedule.Class

newtype OSThreadPool (n :: Nat) a = OSThreadPool {unOSThreadPool :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

data WorkerLink a = WorkerLink
  { jobTChan :: TChan (Maybe (IO a))
  , resultTChan :: TChan a
  }

putJob :: WorkerLink a -> OSThreadPool n a -> IO ()
putJob WorkerLink {..} OSThreadPool {..} =
  atomically $
    writeTChan jobTChan $
      Just unOSThreadPool

makeWorkerLink :: IO (WorkerLink a)
makeWorkerLink = do
  jobTChan <- atomically newTChan
  resultTChan <- atomically newTChan
  let worker = do
        job <- atomically $ readTChan jobTChan
        case job of
          Nothing -> return ()
          Just action -> do
            result <- action
            atomically $ writeTChan resultTChan result
            worker
  void $ forkOS worker
  return WorkerLink {..}

proxyForActions :: NonEmpty (OSThreadPool n a) -> Proxy n
proxyForActions _ = Proxy

instance (KnownNat n, (1 <=? n) ~ True) => MonadSchedule (OSThreadPool n) where
  schedule actions = OSThreadPool $ do
    let n = natVal $ proxyForActions actions
    workerLinks <- replicateM (fromInteger n) makeWorkerLink
    backgroundActions <- forM (zip (cycle workerLinks) (toList actions)) $
      \(link, action) -> do
        putJob link action
        return $ resultTChan link
    pollPools backgroundActions
    where
      pollPools :: [TChan a] -> IO (NonEmpty a, [OSThreadPool n a])
      pollPools chans = do
        results <- traverse pollPool chans
        case partitionEithers results of
          (_, []) -> do
            threadDelay 1000
            pollPools chans
          (remainingChans, a : as) ->
            return
              ( a :| as
              , OSThreadPool . atomically . readTChan <$> remainingChans
              )

      pollPool :: TChan a -> IO (Either (TChan a) a)
      pollPool chan = maybe (Left chan) Right <$> atomically (tryReadTChan chan)
