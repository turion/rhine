-- | Create 'ClSF's with randomness without 'IO'.
--   Uses the @MonadRandom@ package.
--   This module copies the API from @dunai@'s
--   'Control.Monad.Trans.MSF.Random'.

module FRP.Rhine.ClSF.Random
  ( module FRP.Rhine.ClSF.Random
  , module X
  )
  where


-- transformers
import Control.Monad.Trans.Reader

-- MonadRandom
import Control.Monad.Random

-- dunai
import qualified Control.Monad.Trans.MSF.Random as MSF
import Control.Monad.Trans.MSF.Random as X hiding (runRandS, evalRandS)

-- rhine
import FRP.Rhine.ClSF.Core


-- | Commute one 'ReaderT' layer past a 'RandT' layer.
commuteReaderRand :: ReaderT r (RandT g m) a -> RandT g (ReaderT r m) a
commuteReaderRand (ReaderT f) = liftRandT $ \g -> ReaderT $ \r -> runRandT (f r) g

-- | Generates random values, updating the generator on every step.
runRandS
  :: (RandomGen g, Monad m)
  => ClSF (RandT g m) cl a     b
  -> g -- ^ The initial random seed
  -> ClSF          m  cl a (g, b)
runRandS clsf g = MSF.runRandS (liftMSFPurer commuteReaderRand clsf) g

-- | Updates the generator every step but discards the generator.
evalRandS
  :: (RandomGen g, Monad m)
  => ClSF (RandT g m) cl a b
  -> g
  -> ClSF          m  cl a b
evalRandS clsf g = runRandS clsf g >>> arr snd
