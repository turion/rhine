{-# LANGUAGE NamedFieldPuns #-}

module Data.Automaton.Bayes where

-- base
import Control.Arrow

-- transformers
import Control.Monad.Trans.Reader (ReaderT (..))

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Population (PopulationT (..), fromWeightedList, runPopulationT)

-- mmorph
import Control.Monad.Morph (hoist)

-- automaton
import Data.Automaton (Automaton (..), handleAutomaton)
import Data.Stream (StreamT (..))
import Data.Stream.Result (Result (..))

-- | Run the Sequential Monte Carlo algorithm continuously on an 'Automaton'
runPopulationS ::
  forall m a b.
  (Monad m) =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. PopulationT m x -> PopulationT m x) ->
  Automaton (PopulationT m) a b ->
  -- FIXME Why not Automaton m a (PopulationT b)
  Automaton m a [(b, Log Double)]
runPopulationS nParticles resampler =
  handleAutomaton
    ( runPopulationStream
        (commuteReaderPopulation . hoist resampler . commuteReaderPopulationBack)
        . hoist commuteReaderPopulation
    )
  where
    commuteReaderPopulation :: forall m r a. (Monad m) => ReaderT r (PopulationT m) a -> PopulationT (ReaderT r m) a
    commuteReaderPopulation = fromWeightedList . ReaderT . fmap runPopulationT . runReaderT

    commuteReaderPopulationBack :: forall m r a. (Monad m) => PopulationT (ReaderT r m) a -> ReaderT r (PopulationT m) a
    commuteReaderPopulationBack = ReaderT . fmap fromWeightedList . runReaderT . runPopulationT

    runPopulationStream ::
      forall m b.
      (Monad m) =>
      (forall x. PopulationT m x -> PopulationT m x) ->
      StreamT (PopulationT m) b ->
      StreamT m [(b, Log Double)]
    runPopulationStream resampler StreamT {step, state} =
      StreamT
        { state = replicate nParticles (state, 1 / fromIntegral nParticles)
        , step = \states -> do
            resultsAndProbabilities <- runPopulationT $ normalize $ resampler $ do
              state <- fromWeightedList $ pure states
              step state
            return $! Result (first resultState <$> resultsAndProbabilities) (first output <$> resultsAndProbabilities)
        }

-- FIXME see PR re-adding this to monad-bayes
normalize :: (Monad m) => PopulationT m a -> PopulationT m a
normalize = fromWeightedList . fmap (\particles -> second (/ (sum $ snd <$> particles)) <$> particles) . runPopulationT
