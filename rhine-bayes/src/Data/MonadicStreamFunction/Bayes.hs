module Data.MonadicStreamFunction.Bayes where

-- base
import Control.Arrow
import Control.Monad (forM)
import Data.Functor (($>))
import Data.List (transpose)
import Data.Tuple (swap)

-- vector
import Data.Vector (fromList)

-- transformers

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class (MonadDistribution (logCategorical))
import Control.Monad.Bayes.Population
    ( fromWeightedList, runPopulation, spawn, Population)

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
  (forall x. Population m x -> Population m x) ->
  MSF (Population m) a b ->
  -- FIXME Why not MSF m a (Population b)
  MSF m a [(b, Log Double)]
runPopulationS nParticles resampler = runPopulationsS resampler . (spawn nParticles $>)

-- | Run the Sequential Monte Carlo algorithm continuously on a 'Population' of 'MSF's
runPopulationsS ::
  (Monad m) =>
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  Population m (MSF (Population m) a b) ->
  MSF m a [(b, Log Double)]
runPopulationsS resampler = go
  where
    go msfs = MSF $ \a -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      -- FIXME This normalizes, which introduces bias, whatever that means
      bAndMSFs <- runPopulation $ normalize $ resampler $ flip unMSF a =<< msfs
      return $
        second (go . fromWeightedList . return) $
          unzip $
            (swap . fmap fst &&& swap . fmap snd) . swap <$> bAndMSFs

-- | "Particle parameter marginalized Metropolis-Hastings" - adaptation of PMMH
ppmmh ::
  -- | Number of particles parameter
  MonadDistribution m => Int ->
  -- | Number of particles state
  Int ->
  -- | Resampler for parameter
  (forall x . Population m x -> Population m x) ->
  -- | Resampler for state
  (forall x . Population m x -> Population m x) ->
  MSF m a p ->
  MSF (Population m) (a, p) b ->
  MSF m a ([b], [p])
ppmmh nPar nState resPar resState par state = ppmmhS resPar resState (replicate nPar par) (replicate nState state)

ppmmhS ::
  forall m a p b .
  (MonadDistribution m) =>
  -- | Resampler for parameter
  (forall x . Population m x -> Population m x) ->
  -- | Resampler for state
  (forall x . Population m x -> Population m x) ->
  [MSF m a p] ->
  [MSF (Population m) (a, p) b] ->
  MSF m a ([b], [p])
ppmmhS resPar resState = go
  where
    go ::
      MonadDistribution m =>
      [MSF m a p] ->
      [MSF (Population m) (a, p) b] ->
      MSF m a ([b], [p])
    go parMSFs stateMSFs = MSF $ \a -> do
      pars <- forM parMSFs $ flip unMSF a
      bAndStateMSFs <- forM pars $ \(p, _) -> runPopulation $ flip unMSF (a, p) =<< fromWeightedList (pure $ (, 1) <$> stateMSFs)
      let parWeights = sum . fmap snd <$> bAndStateMSFs
      -- FIXME it's not so nice that the next step is in m, but the side effects should all be pure
      parMSFs' <- runPopulation $ resPar $ fromWeightedList $ pure $ zip (snd <$> pars) parWeights
      bAndStateMSFsT <- transpose <$> mapM (runPopulation . normalize . resState . fromWeightedList . pure) bAndStateMSFs
      bAndStateMSFs <- forM bAndStateMSFsT $ \forallParameters -> do
        choice <- logCategorical $ fromList $ snd <$> forallParameters
        pure $ fst $ forallParameters !! choice

      pure ((fst <$> bAndStateMSFs, fst <$> pars), go (fst <$> parMSFs') (snd <$> bAndStateMSFs))

kernelToProcess ::
  Monad m =>
  -- | Initial parameter
  p ->
  -- | Proposition kernel
  (a -> p -> m p) ->
  MSF m a p
kernelToProcess p0 f = feedback p0 $ arrM $ \(a, p) -> dup <$> f a p
  where
    dup p = (p, p)

-- FIXME see PR re-adding this to monad-bayes
normalize :: (Monad m) => Population m a -> Population m a
normalize = fromWeightedList . fmap (\particles -> second (/ (sum $ snd <$> particles)) <$> particles) . runPopulation
