{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

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

import Control.Monad (forM, replicateM)
import Control.Monad.Bayes.Class (MonadDistribution (..), MonadFactor (score))
import Control.Monad.Bayes.Sampler.Lazy (SamplerT)
import Data.Automaton (Automaton (..), handleAutomaton)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Map.Strict (Map, fromList, updateLookupWithKey)
import Data.Stream (StreamT (..))
import Data.Stream.Result (Result (..))
import Data.Stream.Optimized (OptimizedStreamT(Stateful))

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

data Value
  = Int Integer
  | Array [Value]
  | Object (Map Integer Value)

-- FIXME Strings

data Function
  = -- FIXME To get arbitrary lambda calculus to work I probably have to do NBE
    -- = App Function Function

    -- | Lambda Integer Function
    -- | Var Integer
    Id
  | Compose Function Function
  | -- FIXME This is far from a complete API
    At Integer
  | Map Function
  | Over Integer Function
  | Const Value
  | Plus
  | Concat
  | Par Function Function
  | Head
  | Tail

-- FIXME I want more something like a counter how many (and how grave?) type errors have been done, so more a Validation monad
interpret :: Function -> Value -> Maybe Value
interpret Id v = Just v
interpret (Compose f g) x = interpret g x >>= interpret f
interpret (At i) (Array vs) = vs !? i
interpret (At _) _ = Nothing
interpret (Map f) (Array vs) = Array <$> traverse (interpret f) vs
interpret (Map f) (Object m) = Object <$> traverse (interpret f) m
interpret (Map _) _ = Nothing
interpret (Over key f) (Object m) = updateLookupWithKey (const $ interpret f) key m & \(mv, mm) -> mv $> Object mm
interpret (Over _ _) _ = Nothing
interpret (Const v) _ = Just v
interpret Plus (Array [Int v1, Int v2]) = Just $ Int $ v1 + v2
interpret Plus _ = Nothing
-- interpret Concat (Array [String s1, String s2]) = Just $ String $ s1 <> s2 -- FIXME
interpret Concat _ = Nothing
interpret (Par f1 f2) (Array [v1, v2]) = fmap Array $ (\v1' v2' -> [v1', v2']) <$> interpret f1 v1 <*> interpret f2 v2
interpret (Par _ _) _ = Nothing
interpret Head (Array (v : _)) = Just v
interpret Head _ = Nothing
interpret Tail (Array (_ : vs)) = Just $ Array vs
interpret Tail _ = Nothing

fst' :: Function
fst' = Head

snd' :: Function
snd' = Compose Head Tail

(!?) :: [a] -> Integer -> Maybe a
[] !? _ = Nothing
(a : as) !? n
  | n <= 0 = Just a
  | otherwise = as !? (n - 1)

uniformM :: (MonadDistribution m) => [m a] -> m a
uniformM mas = sequence mas >>= uniformD

class Randomly a where
  randomly :: (Monad m) => SamplerT m a

instance Randomly Int where
  randomly = geometric 0.1

instance Randomly Integer where
  randomly = fromIntegral <$> (randomly @Int)

instance (Randomly a, Randomly b) => Randomly (a, b) where
  randomly = (,) <$> randomly <*> randomly

-- FIXME If I can have a more syntactic sampler that is a lazy probability tree,
-- I could just resolve the position of a value in there and then sample by moving around in the tree
class (Randomly a) => Mutate a where
  mutate :: (Monad m) => a -> SamplerT m a

instance Mutate Integer where
  mutate i =
    uniformM
      [ (i +) <$> randomly
      , (i -) <$> randomly
      ]

instance Randomly Value where
  randomly =
    uniformM
      [ -- FIXME Strings
        Int <$> randomly
      , Array <$> do
          n <- randomly
          replicateM n randomly
      , Object . fromList <$> do
          n <- randomly
          -- FIXME Caution this replicates the same key several times, rather make an ascending list
          replicateM n $ (,) <$> randomly <*> randomly
      ]

instance Mutate Value where
  mutate (Int i) =
    uniformM
      [ Int <$> mutate i
      , randomly
      ]
  mutate (Array vs) =
    uniformM
      [ Array <$> do
          let l = length vs
          forM vs $ \v -> do
            b <- bernoulli $ 1 / fromIntegral l
            if b then mutate v else pure v
      , randomly
      ]
  mutate (Object m) =
    uniformM
      [ Object <$> do
          let l = length m
          forM m $ \v -> do
            b <- bernoulli $ 1 / fromIntegral l
            if b then mutate v else pure v
      , randomly
      ]

-- FIXME refactor with Compose traversable

-- FIXME check whether geometric distribution is the right one
diffuse :: (Monad m, Mutate a) => Double -> a -> SamplerT m a
diffuse p a = do
  n <- geometric p
  repeatedly n mutate a
  where
    repeatedly :: (Monad m) => Int -> (a -> m a) -> a -> m a
    repeatedly n f a
      | n <= 0 = return a
      | otherwise = f a >>= repeatedly (n - 1) f

-- FIXME derive from mutate somehow? (This should be the fixpoint of mutate)
-- FIXME this should be generically derivable with a type class
instance Randomly Function where
  randomly =
    uniformM
      [ pure Id
      , Compose <$> randomly <*> randomly
      , At <$> randomly
      , Map <$> randomly
      , Over <$> randomly <*> randomly
      , Const <$> randomly
      , pure Plus
      , pure Concat
      , Par <$> randomly <*> randomly
      , pure Head
      , pure Tail
      ]

instance Mutate Function where
  mutate Id = randomly
  mutate (Compose f1 f2) =
    uniformM
      [ Compose f1 <$> mutate f2
      , flip Compose f2 <$> mutate f1
      , Compose <$> mutate f1 <*> mutate f2
      , randomly
      ]
  mutate (At i) =
    uniformM
      [ At <$> mutate i
      , randomly
      ]
  mutate (Map f) =
    uniformM
      [ Map <$> mutate f
      , randomly
      ]
  mutate (Over i f) =
    uniformM
      [ Over i <$> mutate f
      , flip Over f <$> mutate i
      , Over <$> mutate i <*> mutate f
      , randomly
      ]
  mutate (Const v) =
    uniformM
      [ Const <$> mutate v
      , randomly
      ]
  mutate Plus = randomly
  mutate Concat = randomly
  mutate (Par f1 f2) =
    uniformM
      [ Compose f1 <$> mutate f2
      , flip Compose f2 <$> mutate f1
      , Compose <$> mutate f1 <*> mutate f2
      , randomly
      ]
  mutate Head = randomly
  mutate Tail = randomly

-- FIXME refactor. Also I don't want id on first sample
universalPrior :: MonadFactor m => Automaton (SamplerT m) Value (Maybe Value)
universalPrior = Automaton $ Stateful $ StreamT
  { state = Nothing
  , step = \case
      Nothing -> ReaderT $ \a -> flip Result (Just a) . Just <$> randomly @(Value, Function)
      Just (s, f) -> ReaderT $ \a -> do
        let result = do
              s'b <- interpret f $ Array [s, a]
              s' <- interpret fst' s'b
              b <- interpret snd' s'b
              return (s', b)

        case result of
          Just (s', b) -> do
            s'' <- mutate s'
            f' <- mutate f
            return $ Result (Just (s'', f')) (Just b)
          -- FIXME I need to sample SamplerT here. Or do the scoring outside and return Nothing here
          Nothing -> Result Nothing Nothing <$ score 0.1
  }

-- FIXME Distance function on values

-- FIXME Typed variant with Conor McBride's slogan "Index over the past, fiber over the future"
