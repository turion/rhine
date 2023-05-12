module Main where

-- hspec
import Test.Hspec

-- rhine-bayes
import qualified Oscillator

main :: IO ()
main = hspec $ do
  Oscillator.test
