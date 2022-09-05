module Data.MonadicStreamFunction.Bayes where

-- base
import Control.Arrow
import Control.Monad (forM, join, replicateM)
import Data.Functor (($>), (<&>))
import Data.Functor.Compose
import Data.Functor.Identity (Identity (Identity))
import Data.List (sortOn)
import Data.Proxy
import Data.Tuple (swap)
import GHC.TypeNats

-- transformers
import Control.Monad.Trans.Class

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Weighted

-- dunai
import Control.Monad.Trans.MSF (performOnFirstSample)
import Control.Monad.Trans.MSF.List
import Control.Monad.Trans.MSF.Reader (readerS, runReaderS_)
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF (..))

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

-- FIXME see PR re-adding this to monad-bayes
normalize :: Monad m => Population m a -> Population m a
normalize = fromWeightedList . fmap (\particles -> second (/ (sum $ snd <$> particles)) <$> particles) . runPopulation

class SoftEq a where
  -- | `similarity a1 a2 == 1` if they are exactly equal, and 0 if they are completely different.
  similarity :: a -> a -> Log Double

  -- | Scores the similarity of the two inputs
  (=~) :: MonadMeasure m => a -> a -> m ()
  a1 =~ a2 = score $ similarity a1 a2

-- FIXME Do I want this?
instance SoftEq Double where
  similarity a1 a2 = normalPdf a1 1 a2

-- | Hard equality check
newtype Exact a = Exact {getExact :: a}
  deriving (Eq, Show, Read, Ord, Enum, Functor, Num, Integral, Real, Fractional, Floating)

instance Eq a => SoftEq (Exact a) where
  similarity a1 a2 = if a1 == a2 then 1 else 0

-- FIXME naming
newtype DigitsPrecision (n :: Nat) a = DigitsPrecision {getDigitsPrecision :: a}
  deriving (Eq, Show, Read, Ord, Enum, Functor, Num, Integral, Real, Fractional, Floating)

digitsPrecisionProxy :: DigitsPrecision n a -> Proxy n
digitsPrecisionProxy _ = Proxy

instance KnownNat n => SoftEq (DigitsPrecision n Double) where
  similarity a1 a2 = normalPdf (getDigitsPrecision a1) (0.1 ^ natVal (digitsPrecisionProxy a1)) (getDigitsPrecision a2)

instance (SoftEq a, SoftEq b) => SoftEq (a, b) where
  similarity (a1, b1) (a2, b2) = similarity a1 a2 + similarity b1 b2 -- FIXME + not *?

-- TODO particle path finder:

-- * Send particles in all directions

-- * Move in the average direction where particles came closest

-- This should work for general control if the control space is not too high dimensional (bang bang to reduce to graph problem)

-- * Is there a MCMC control algorithm?

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

runPopulationS' ::
  forall m a b.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  MSF (Population m) a b ->
  -- FIXME Why not MSF m a (Population b)
  MSF m a [(b, Log Double)]
runPopulationS' nParticles resampler msf = widthFirst $ fmap swap $ handleCompose'''''' $ hoistMSF (massagePopulation . normalize . resampler) $ performOnFirstSample $ spawn nParticles $> msf

runPopulationS'' ::
  forall m a b.
  Monad m =>
  MSF (Population m) a b ->
  MSF m a [(b, Log Double)]
runPopulationS'' = runPopulationS''' . return
 where
  runPopulationS''' :: Monad m => Population m (MSF (Population m) a b) -> MSF m a [(b, Log Double)]
  runPopulationS''' msfs = MSF $ \a -> do
    -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
    -- FIXME This normalizes, which introduces bias, whatever that means
    bAndMSFs <- runPopulation $ flip unMSF a =<< msfs
    return
      $ second
        ( runPopulationS'''
            . fromWeightedList
            . return
        )
      $ unzip
      $ (swap . fmap fst &&& swap . fmap snd) . swap <$> bAndMSFs

runPopulationS''' ::
  forall m a b.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  MSF (Population m) a b ->
  -- FIXME Why not MSF m a (Population b)
  MSF m a [(b, Log Double)]
runPopulationS''' nParticles resampler = runPopulationS'' . hoistMSF (normalize . resampler) . performOnFirstSample . (spawn nParticles $>)

-- | This is stupid because it runs into the collapse
runPopulationParamSimpleS ::
  forall m a b param.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  m param ->
  MSF (Population m) (param, a) b ->
  -- FIXME Why not MSF m a (Population b)
  MSF m a [((b, param), Log Double)]
runPopulationParamSimpleS nParticles resampler param = runPopulationsS resampler . ((spawn nParticles >> lift param) <&>) . runReaderS_ . readerS . (&&& arr fst)

-- | Constant number of particles
runPopulationParamDirichletConstant ::
  forall m a b param.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  m param ->
  MSF (Population m) (param, a) b ->
  -- FIXME Why not MSF m a (Population b)
  MSF m (Log Double, a) [((b, param), Log Double)]
runPopulationParamDirichletConstant nParticles resampler param = runPopulationParamDirichletConstant' . mkInitialMSFs
 where
  mkInitialMSFs ::
    MSF (Population m) (param, a) b ->
    Population m (param, MSF (Population m) (param, a) b)
  mkInitialMSFs msf = do
    spawn nParticles
    initialParam <- lift param
    return (initialParam, msf)

  runPopulationParamDirichletConstant' ::
    Monad m =>
    Population m (param, MSF (Population m) (param, a) b) ->
    MSF m (Log Double, a) [((b, param), Log Double)]
  runPopulationParamDirichletConstant' paramAndMSFs = MSF $ \(pNew, a) -> do
    -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
    -- FIXME This normalizes, which introduces bias, whatever that means
    bAndMSFs <- runPopulation $ do
      (param, msf) <- paramAndMSFs
      bAndMSFs <- unMSF msf (param, a)
      return (param, bAndMSFs)
    newParam <- param
    -- FIXME I should add the new sample before resampling, otherwise it will never have a chance against the 1/nParticles weight!
    -- Or is it fine because I'm weighting before resampling as well?
    -- There is a difference because we're not taking the total weight into account
    let _ : bAndMSFsKept = sortOn snd bAndMSFs
        ((_overwritten, bestBAndMSF), _p) = last bAndMSFsKept
        -- I'm unsure about the normalization here, the total weight is 1 + pNew
        bAndMSFsWithNew = ((newParam, bestBAndMSF), pNew) : bAndMSFsKept
    return $
      second
        (runPopulationParamDirichletConstant' . normalize . resampler . fromWeightedList . return)
        ( unzip $
            ( \((param, (b, msf)), p) ->
                ( ((b, param), p)
                , ((param, msf), p)
                )
            )
              <$> bAndMSFsWithNew
        )

-- | Constant number of particles
runPopulationParamDirichletElastic ::
  forall m a b param.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  m param ->
  MSF (Population m) (param, a) b ->
  MSF m (Int, a) [((b, param), Log Double)]
runPopulationParamDirichletElastic nParticles resampler param = runPopulationParamDirichletElastic' . mkInitialMSFs
 where
  mkInitialMSFs ::
    MSF (Population m) (param, a) b ->
    Population m (param, MSF (Population m) (param, a) b)
  mkInitialMSFs msf = do
    spawn nParticles
    initialParam <- lift param
    return (initialParam, msf)

  runPopulationParamDirichletElastic' ::
    Monad m =>
    Population m (param, MSF (Population m) (param, a) b) ->
    MSF m (Int, a) [((b, param), Log Double)]
  runPopulationParamDirichletElastic' paramAndMSFs = MSF $ \(n, a) -> do
    -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
    -- FIXME This normalizes, which introduces bias, whatever that means
    bAndMSFs <- runPopulation $ do
      (param, msfs) <- paramAndMSFs
      bAndMSFs <- unMSF msfs (param, a)
      return (param, bAndMSFs)
    newParams <- replicateM n param
    let ((_overwritten, bestBAndMSF), _p) = last $ sortOn snd bAndMSFs
        bAndMSFsWithNew = map (\newParam -> ((newParam, bestBAndMSF), 1)) newParams ++ bAndMSFs
    return $
      second
        (runPopulationParamDirichletElastic' . normalize . resampler . fromWeightedList . return)
        ( unzip $
            ( \((param, (b, msf)), p) ->
                ( ((b, param), p)
                , ((param, msf), p)
                )
            )
              <$> bAndMSFsWithNew
        )

-- FIXME PR to dunai to simplify type signature
hoistMSF :: Functor m => (forall x. m x -> n x) -> MSF m a b -> MSF n a b
hoistMSF morph msf = MSF $ \a -> morph $ fmap (hoistMSF morph) <$> unMSF msf a

massagePopulation :: Functor m => Population m a -> Compose (ListT m) ((,) (Log Double)) a
massagePopulation = Compose . ListT . fmap (map swap) . runPopulation

-- FIXME see PR re-adding this to monad-bayes
normalize :: Monad m => Population m a -> Population m a
normalize = fromWeightedList . fmap (\particles -> second (/ (sum $ snd <$> particles)) <$> particles) . runPopulation

constantParameter ::
  Monad m =>
  m a ->
  MSF m arbitrary a
constantParameter action = performOnFirstSample $ pure <$> action

{-
handle' :: (Monad m, Monad (t m), Functor t') => (forall x . t m x -> m (t' x)) -> t m (MSF (t m) a b) -> MSF m a (t' b)
handle' handler = handleCompose . fmap (morphS (Compose . handler)) . Compose . handler
-}

handle :: (Functor m, Monad m, Monad (t m)) => (forall x. t m x -> m (t' x)) -> t m (MSF (t m) a b) -> MSF m a (t' b)
handle handler msf = MSF $ \a -> do
  let tb = fst <$> (flip unMSF a =<< msf)
      msf' = snd <$> (flip unMSF a =<< msf)
  b <- handler tb
  return (b, handle handler msf')

{-
handle'' :: (Monad m, Monad (t m)) => (forall x . t m x -> m (t' x)) -> t m (MSF (t m) a b) -> MSF m a (t' b)
handle'' handler = _ . joinS
-}

joinOutput :: Monad m => MSF m a (m b) -> MSF m a b
joinOutput msf = msf >>> arrM id

joinS :: Monad m => m (MSF m a b) -> MSF m a (m b)
joinS msf = MSF $ \a -> do
  let joined = flip unMSF a =<< msf
      tb = fst <$> joined
      msf' = snd <$> joined
  return (tb, joinS msf')

handleCompose :: (Monad m, Monad (Compose m f), Functor f) => Compose m f (MSF (Compose m f) a b) -> MSF m a (f b)
handleCompose msf = MSF $ \a -> do
  bAndMSF <- getCompose $ flip unMSF a =<< msf
  return (fst <$> bAndMSF, handleCompose $ Compose $ return $ snd <$> bAndMSF)

newtype FlipCompose f m a = FlipCompose {getFlipCompose :: Compose m f a}
  deriving (Functor, Applicative)

-- Could prove Monad (FlipCompose f m)
handleCompose'''' :: (Functor f, Monad m, Monad (Compose m f), Functor m, Monad (FlipCompose f m)) => Compose m f (MSF (Compose m f) a b) -> MSF m a (f b)
handleCompose'''' = handle (getCompose . getFlipCompose) . fmap (morphS FlipCompose) . FlipCompose

handleCompose' :: (Monad m, Monad (Compose m f), Functor f) => MSF (Compose m f) a b -> MSF m a (f b)
handleCompose' = handleCompose . return

handleCompose'' :: (Monad m, Monad (Compose m f), Functor f) => MSF (Compose m f) a b -> MSF m a (f b)
handleCompose'' msf = MSF $ \a -> do
  bAndMSF <- getCompose $ flip unMSF a msf
  return (fst <$> bAndMSF, handleCompose $ Compose $ return $ snd <$> bAndMSF)

{-
handleCompose''' :: (Monad m, Monad (Compose m f), Functor f) => MSF (Compose m f) a b -> MSF m a (f b)
handleCompose''' = _ . (arr getCompose <<<) . snapshot
-}

-- TODO this is basically saying that f is some kind of list
handleCompose''''' :: (Monad m, Traversable f, Monad f) => f (MSF (Compose m f) a b) -> MSF m a (f b)
handleCompose''''' fmsf = MSF $ \a -> fmap ((fmap fst &&& (handleCompose''''' . fmap snd)) . join) $ forM fmsf $ getCompose . flip unMSF a

handleCompose'''''' :: (Monad m, Traversable f, Monad f) => MSF (Compose m f) a b -> MSF m a (f b)
handleCompose'''''' = handleCompose''''' . return

-- FIXME Is this handleCompose for Compose m Identity or Compose Identity m?

-- | Keep running the 'MSF', but return the output in the current context.
snapshot :: Functor m => MSF m a b -> MSF m a (m b)
snapshot msf = MSF $ \a -> do
  let b = fst <$> unMSF msf a
      msf' = snd <$> unMSF msf a
  (b,) . snapshot <$> msf'

{-
snapshot' :: (Functor m, Monad m, Monad (Compose Identity m)) => MSF m a b -> MSF m a (m b)
snapshot' = _ . handleCompose' . morphS (Compose . Identity)
-}

snapshot'' :: (Functor m, Monad m, Monad (Compose m m)) => MSF m a b -> MSF m a (m b)
snapshot'' = handleCompose' . morphS (Compose . fmap return)

{-
Needs Monad f or similar again
snapshotCompose :: Monad m => MSF (Compose m f) a b -> MSF m a (f b)
snapshotCompose msf = MSF $ \a -> do
  fBsAndContinuations <- getCompose $ unMSF msf a
  _
-}

accumulate' :: (Functor m, Monad m, Monad (Compose Identity m)) => MSF m a b -> MSF Identity a (m b)
accumulate' = handleCompose' . morphS (Compose . Identity)

collapseS :: MonadMeasure m => MSF (Population m) a b -> MSF m a b
collapseS = morphS collapse

-- FIXME unit test. Does this what I think it does?
properS :: MonadDistribution m => MSF (Population m) a b -> MSF (Weighted m) a b
properS = morphS proper

-- FIXME separate module. actually, separate package
-- And search whether such a package already exists first.
class Statistical a where
  statistic :: [(a, Double)] -> a

newtype Average a = Average {getAverage :: a}
  deriving (Num, Fractional)

instance Fractional a => Statistical (Average a) where
  -- FIXME realToFrac isn't nice, but unfortunately we're stuck with Doubles
  statistic = sum . fmap (uncurry (*) . second realToFrac)

-- FIXME try coerce
average :: Fractional a => [(a, Double)] -> a
average = getAverage . statistic . fmap (first Average)
