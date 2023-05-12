module Oscillator where

-- transformers
import Control.Monad.Trans.Class

-- hspec
import Test.Hspec

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler.Strict (sampleIOfixed)

-- rhine
import FRP.Rhine

-- rhine-bayes
import Model.Oscillator
import FRP.Rhine.Reactimation.ClockErasure (eraseClockClSF)

cl :: RescaledClock Count Double
cl = RescaledClock
  { unscaledClock = FixedStep
  , rescale = fromInteger
  }

model :: MonadDistribution m => m (MSF m () (Maybe (Sensor, Pos)))
model = eraseClock $ arr (const 7) >-> genModelWithoutTemperature @@ cl

test = describe "Oscillator" $ do
  describe "Model" $ do
    it "produces the same samples" $ do
      samples <- sampleIOfixed $ do
        modelRunning <- model
        embed modelRunning $ replicate 5 ()
      samples `shouldBe` [Just ((-4.806787835816406e-2,19.10640521599688),(-1.1617872663777717,18.42559797345118)),Just ((-11.146120243656256,-27.246593098950296),(-10.925158906218407,-26.861788805802437)),Just ((5.486329154017721,8.10295338295754),(5.997914089827855,9.733043553206468)),Just ((2.0804846939225157,-0.8724846124389277),(2.125471590193796,-0.4027738437261448)),Just ((-7.927570850547975,-8.848931383705574),(-8.82156324064908,-9.130390501712256))]
  -- describe "Inference" $ do
  --   _
