{-# LANGUAGE FlexibleInstances #-}

-- test-framework
import Test.Framework

-- monad-schedule (test)
import qualified FreeAsync
import qualified Trans
import qualified Yield

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ FreeAsync.tests
  , Trans.tests
  , Yield.tests
  ]
