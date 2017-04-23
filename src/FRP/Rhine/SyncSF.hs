{-# LANGUAGE Arrows       #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.SyncSF where

-- transformers
import Control.Monad.Trans.Reader (ReaderT, ask, asks)

-- dunai
import Data.MonadicStreamFunction (MSF, liftMSFTrans, arrM, arrM_, sumFrom)
import Data.VectorSpace

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.TimeDomain


type SyncSF m c a b = MSF (ReaderT (TimeInfo c) m) a b

timeless :: Monad m => MSF m a b -> SyncSF m c a b
timeless = liftMSFTrans

arrMSync :: Monad m => (a -> m b) -> SyncSF m c a b
arrMSync = timeless . arrM

arrMSync_ :: Monad m => m b -> SyncSF m c a b
arrMSync_ = timeless . arrM_

timeInfo :: Monad m => SyncSF m c a (TimeInfo c)
timeInfo = arrM_ ask

timeInfoOf :: Monad m => (TimeInfo c -> b) -> SyncSF m c a b
timeInfoOf f = arrM_ $ asks f

integralFrom :: (Monad m, VectorSpace v, Groundfield v ~ Diff (TimeDomainOf c))
             => v -> SyncSF m c v v
integralFrom v0 = proc v -> do
  _sinceTick <- timeInfoOf sinceTick -< ()
  sumFrom v0 -< _sinceTick *^ v

integral :: (Monad m, VectorSpace v, Groundfield v ~ Diff (TimeDomainOf c))
         => SyncSF m c v v
integral = integralFrom zeroVector
