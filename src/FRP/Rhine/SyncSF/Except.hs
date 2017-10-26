module FRP.Rhine.SyncSF.Except where

-- dunai
import Control.Monad.Trans.MSF.Except (Empty)
import qualified Control.Monad.Trans.MSF.Except as MSFE

newtype SyncExcept m cl a b e = SyncExcept (MSFE.MSFExcept (ReaderT (TimeInfo cl)) m a b e)
  deriving Monad

-- need one commutation between ExceptT and ReaderT
commuteReaderExcept :: ReaderT r (ExceptT e m) a -> ExceptT e (ReaderT r m) a
commuteReaderExcept a = ExceptT $ ReaderT $ \r -> runExceptT $ runReaderT a r


try :: SyncSF (ExceptT e m) cl a b -> SyncExcept m cl a b e
try = SyncExcept . liftMSFPurer commuteReaderExcept

safe :: SyncSF m cl a b -> SyncExcept m cl a b e
safe = try . liftMSFTrans
