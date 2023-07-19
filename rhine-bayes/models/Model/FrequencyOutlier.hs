module Model.FrequencyOutlier where

-- rhine
import FRP.Rhine

-- rhine-bayes
import FRP.Rhine.Bayes
import Control.Monad.Bayes.Class

-- | The frequency in Millihertz
newtype Frequency = Frequency { getFrequency :: Double }
  deriving Num

instance VectorSpace Frequency Double where
  zeroVector = 0
  d *^ f = Frequency d * f
  (^+^) = (+)
  dot = (getFrequency .) . (*)

-- FIXME make better (multiple events) and add to library
poissonStupidException :: (MonadDistribution m, Real (Diff td), Fractional (Diff td)) => Diff td -> BehaviourF (ExceptT e m) td e ()
poissonStupidException rate = proc e -> do
  nEvents <- poissonHomogeneous rate -< ()
  if nEvents > 0
    then throwS -< e
    else returnA -< ()

-- FIXME put into lib?
-- | An integral where the integrated value dies of exponentially
decayIntegral :: (VectorSpace v (Diff td), Monad m, Floating (Diff td)) => Diff td -> BehaviourF m td v v
decayIntegral timeConstant = (timeConstant *^) <$> average timeConstant


gridFrequency :: (Diff td ~ Double, MonadDistribution m) => Frequency -> BehaviourF m td () Frequency
gridFrequency gridSetpoint = wiener 0.1 >>> arr (Frequency . (* 20)) >>> decayIntegral 10 >>> arr (+ gridSetpoint)

data FailureModes = Working | Splat

frequencyMeasurement :: (Diff td ~ Double, MonadDistribution m) => Frequency -> BehaviourF m td () Frequency
frequencyMeasurement gridSetpoint = gridFrequency gridSetpoint >>> safely working
 where
  justWorking = proc theGridFrequency -> do
    poissonStupidException 60 -< Splat
    noise <- arr Frequency <<< whiteNoise 5 -< ()
    returnA -< theGridFrequency + noise
  working = do
    try justWorking
    randomFrequency <- Frequency <$> normal 50000 1000
    splat randomFrequency
  justSplat randomFrequency = proc _ -> do
    poissonStupidException 1 -< Working
    returnA -< randomFrequency
  splat randomFrequency = try (justSplat randomFrequency) *> working
