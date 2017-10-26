{-# LANGUAGE Arrows #-}
module FRP.Rhine.SyncSF.Except
  ( module FRP.Rhine.SyncSF.Except
  , module X
  , safe, safely, Empty, exceptS, runMSFExcept, step
  -- TODO Much useful exports... very boiler plate
  )
  where

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except as X
import Control.Monad.Trans.Reader

-- dunai
import Control.Monad.Trans.MSF.Except hiding (try, once, once_, throwOn, throwOn', throwS)
-- TODO Find out whether there is a cleverer way to export all of that
import qualified Control.Monad.Trans.MSF.Except as MSFE

-- rhine
import FRP.Rhine

--newtype SyncExcept m cl a b e = SyncExcept (MSFE.MSFExcept (ReaderT (TimeInfo cl) m) a b e)
--  deriving (Functor, Applicative, Monad)

type SyncExcept m cl a b e = MSFExcept (ReaderT (TimeInfo cl) m) a b e

-- need one commutation between ExceptT and ReaderT
commuteReaderExcept :: ReaderT r (ExceptT e m) a -> ExceptT e (ReaderT r m) a
commuteReaderExcept a = ExceptT $ ReaderT $ \r -> runExceptT $ runReaderT a r


try :: Monad m => SyncSF (ExceptT e m) cl a b -> SyncExcept m cl a b e
try = MSFE.try . liftMSFPurer commuteReaderExcept

once :: Monad m => (a -> m e) -> SyncExcept m cl a b e
once f = MSFE.once $ lift . f

once_ :: Monad m => m e -> SyncExcept m cl a b e
once_ = once . const

throwS :: Monad m => SyncSF (ExceptT e m) cl e a
throwS = arrMSync throwE

throwOn' :: Monad m => SyncSF (ExceptT e m) cl (Bool, e) ()
throwOn' = proc (b, e) -> if b
  then throwS  -< e
  else returnA -< ()

throwOn :: Monad m => e -> SyncSF (ExceptT e m) cl Bool ()
throwOn e = proc b -> throwOn' -< (b, e)
