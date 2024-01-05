module Data.MonadicStreamFunction.Bayes where

-- base
import Control.Arrow
import Data.Functor (($>))
import Data.Tuple (swap)

-- transformers

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Population

-- dunai
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF (..))

-- | Run the Sequential Monte Carlo algorithm continuously on an 'MSF'
runPopulationS ::
  forall m a b.
  (Monad m) =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. PopulationT m x -> PopulationT m x) ->
  MSF (PopulationT m) a b ->
  -- FIXME Why not MSF m a (PopulationT b)
  MSF m a [(b, Log Double)]
runPopulationS nParticles resampler = runPopulationsS resampler . (spawn nParticles $>)

-- | Run the Sequential Monte Carlo algorithm continuously on a 'PopulationT' of 'MSF's
runPopulationsS ::
  (Monad m) =>
  -- | Resampler
  (forall x. PopulationT m x -> PopulationT m x) ->
  PopulationT m (MSF (PopulationT m) a b) ->
  MSF m a [(b, Log Double)]
runPopulationsS resampler = go
  where
    go msfs = MSF $ \a -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      -- FIXME This normalizes, which introduces bias, whatever that means
      bAndMSFs <- runPopulationT $ normalize $ resampler $ flip unMSF a =<< msfs
      return $
        second (go . fromWeightedList . return) $
          unzip $
            (swap . fmap fst &&& swap . fmap snd) . swap <$> bAndMSFs

-- FIXME see PR re-adding this to monad-bayes
normalize :: (Monad m) => PopulationT m a -> PopulationT m a
normalize = fromWeightedList . fmap (\particles -> second (/ (sum $ snd <$> particles)) <$> particles) . runPopulationT
