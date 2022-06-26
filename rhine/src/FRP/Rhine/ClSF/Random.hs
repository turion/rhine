{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
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
import Control.Monad.IO.Class

-- MonadRandom
import Control.Monad.Random

-- dunai
import Control.Monad.Trans.MSF.Except (performOnFirstSample)
import qualified Control.Monad.Trans.MSF.Random as MSF
import Control.Monad.Trans.MSF.Random as X hiding (runRandS, evalRandS, getRandomS, getRandomRS, getRandomRS_)

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.ClSF.Random.Util

-- * Generating random values from the 'RandT' transformer

-- | Generates random values, updating the generator on every step.
runRandS
  :: (RandomGen g, Monad m)
  => ClSF (RandT g m) cl a     b
  -> g -- ^ The initial random seed
  -> ClSF          m  cl a (g, b)
runRandS clsf g = MSF.runRandS (morphS commuteReaderRand clsf) g

-- | Updates the generator every step but discards the generator.
evalRandS
  :: (RandomGen g, Monad m)
  => ClSF (RandT g m) cl a b
  -> g
  -> ClSF          m  cl a b
evalRandS clsf g = runRandS clsf g >>> arr snd

-- | Updates the generator every step but discards the value,
--   only outputting the generator.
execRandS
  :: (RandomGen g, Monad m)
  => ClSF (RandT g m) cl a b
  -> g
  -> ClSF          m  cl a g
execRandS clsf g = runRandS clsf g >>> arr fst

-- | Evaluates the random computation by using the global random generator.
evalRandIOS
  :: Monad m
  =>     ClSF (RandT StdGen m) cl a b
  -> IO (ClSF               m  cl a b)
evalRandIOS clsf = do
  g <- newStdGen
  return $ evalRandS clsf g

-- | Evaluates the random computation by using the global random generator on the first tick.
evalRandIOS'
  :: MonadIO m
  => ClSF (RandT StdGen m) cl a b
  -> ClSF               m  cl a b
evalRandIOS' = performOnFirstSample . liftIO . evalRandIOS

-- * Creating random behaviours

-- | Produce a random value at every tick.
getRandomS
  :: (MonadRandom m, Random a)
  => Behaviour m time a
getRandomS = constMCl getRandom

-- | Produce a random value at every tick,
--   within a range given per tick.
getRandomRS
  :: (MonadRandom m, Random a)
  => BehaviourF m time (a, a) a
getRandomRS = arrMCl getRandomR

-- | Produce a random value at every tick,
--   within a range given once.
getRandomRS_
  :: (MonadRandom m, Random a)
  => (a, a)
  -> Behaviour m time a
getRandomRS_ range = constMCl $ getRandomR range
