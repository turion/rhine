{-| Utilities for 'FRP.Rhine.ClSF.Except' that need not be exported.
-}
module FRP.Rhine.ClSF.Except.Util where

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader


-- | Commute the effects of the 'ReaderT' and the 'ExceptT' monad.
commuteReaderExcept :: ReaderT r (ExceptT e m) a -> ExceptT e (ReaderT r m) a
commuteReaderExcept a = ExceptT $ ReaderT $ \r -> runExceptT $ runReaderT a r
