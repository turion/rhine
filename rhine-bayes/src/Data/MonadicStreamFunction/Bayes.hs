-- FIXME or should I indeed define everything for automata first and add MSFs separately?
{-# LANGUAGE NamedFieldPuns #-}

module Data.MonadicStreamFunction.Bayes where

-- base
import Control.Arrow

-- transformers
import Control.Monad.Trans.Reader (ReaderT (..))

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Population

-- rhine
import Data.Automaton (AutomatonT (..), Result (..))
import Data.Automaton.MSF (MSF (..))

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
runPopulationS nParticles resampler (MSF AutomatonT {state, step}) =
  MSF
    AutomatonT
      { -- FIXME it's weird that the particles don't have a weight?
        state = replicate nParticles (state, 1 / fromIntegral nParticles)
      , step = \states -> ReaderT $ \a -> do
          -- FIXME This normalizes, which introduces bias, whatever that means
          x <- runPopulationT $ normalize $ resampler $ do
            -- Would it make more sense to put the resampler here?
            state <- fromWeightedList $ pure states
            runReaderT (step state) a
          return $! Result (first resultState <$> x) (first output <$> x)
      }

-- FIXME see PR re-adding this to monad-bayes
normalize :: (Monad m) => PopulationT m a -> PopulationT m a
normalize = fromWeightedList . fmap (\particles -> second (/ (sum $ snd <$> particles)) <$> particles) . runPopulationT
