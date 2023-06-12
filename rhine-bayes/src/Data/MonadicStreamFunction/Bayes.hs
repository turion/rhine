{-# LANGUAGE BangPatterns #-}
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
import Control.Parallel.Strategies (withStrategy, parList, rseq, r0, seqList, evalTuple2, parTuple2)

-- | Run the Sequential Monte Carlo algorithm continuously on an 'MSF'
runPopulationS ::
  forall m a b.
  Monad m =>
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
  Monad m =>
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  Population m (MSF (Population m) a b) ->
  MSF m a [(b, Log Double)]
runPopulationsS resampler = feedback 0 . go
  where
    go msfs = MSF $ \(a, n) -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      -- FIXME This normalizes, which introduces bias, whatever that means
      let doThing = normalize . resampler
      -- let doThing = if n `mod` 5 == 0 then normalize . resampler else id
      !bAndMSFs <- runPopulation $ doThing $ flip unMSF a =<< msfs
      return $ forceTuple $ first ((,n + 1)) $
        second (go . fromWeightedList . return . forceList) $
          unzip $
            -- (swap . fmap fst &&& swap . fmap snd) . swap <$> bAndMSFs
            (swap . fmap fst &&& swap . fmap snd) . swap <$> forceList (withStrategy (parList $ parTuple2 (parTuple2 rseq rseq) rseq) (forceList bAndMSFs))

forceTuple :: ((a, b), c) -> ((a, b), c)
forceTuple ((a, b), c) = a `seq` b `seq` c `seq` ((a, b), c)

forceList :: [a] -> [a]
forceList [] = []
forceList (a : as) = let as' = forceList as in a `seq` as' `seq` (a : as)

-- FIXME see PR re-adding this to monad-bayes
normalize :: Monad m => Population m a -> Population m a
normalize = fromWeightedList . fmap (\particles -> let totalWeight = sum $ snd <$> particles in second (/ totalWeight) <$> particles) . runPopulation
