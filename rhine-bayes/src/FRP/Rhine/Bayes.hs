{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.Rhine.Bayes where

-- base
import Control.Monad (join)
import Data.Functor (($>))

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (mapReaderT, ask, runReaderT)

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Weighted hiding (flatten)

-- dunai
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import qualified Control.Monad.Trans.MSF.Reader as DunaiReader

-- dunai
import Data.MonadicStreamFunction.Bayes (SoftEq)
import qualified Data.MonadicStreamFunction.Bayes as DunaiBayes

-- rhine
import FRP.Rhine hiding (normalize)

-- FIXME Does this haddock work?
-- | See 'Dunai.bayesFilter''
bayesFilter' :: (MonadInfer m, SoftEq sensor) =>
  -- | model
  ClSF m cl input (sensor, state) ->
  -- | external sensor, data source
  ClSF m cl input sensor ->
  ClSF m cl input (sensor, state)
bayesFilter' = DunaiBayes.bayesFilter'

-- FIXME Does this haddock work?
-- | See 'Dunai.bayesFilter'
bayesFilter :: (MonadInfer m, SoftEq sensor) =>
  ClSF m cl input (sensor, latent) ->
  -- | external sensor, data source
  ClSF m cl (input, sensor) latent
bayesFilter = DunaiBayes.bayesFilter

runPopulationCl :: forall m cl a b . Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x . Population m x -> Population m x)
  -> ClSF (Population m) cl a b
  -> ClSF m cl a [(b, Log Double)]
runPopulationCl nParticles resampler = DunaiReader.readerS . DunaiBayes.runPopulationS nParticles resampler . DunaiReader.runReaderS

collapseCl :: MonadInfer m => ClSF (Population m) cl a b -> ClSF m cl a b
collapseCl = hoistClSF collapse

-- FIXME unit test. Does this what I think it does?
properCl :: MonadSample m => ClSF (Population m) cl a b -> ClSF (Weighted m) cl a b
properCl = hoistClSF proper
