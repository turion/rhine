module FRP.Rhine.ClSF.Random.Util where

-- transformers
import Control.Monad.Trans.Reader

-- MonadRandom
import Control.Monad.Random

-- | Commute one 'ReaderT' layer past a 'RandT' layer.
commuteReaderRand :: ReaderT r (RandT g m) a -> RandT g (ReaderT r m) a
commuteReaderRand (ReaderT f) = liftRandT $ \g -> ReaderT $ \r -> runRandT (f r) g
