{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module FreeAsync where

-- base
import Control.Concurrent (newMVar, takeMVar)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (fromList)
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)

-- time
import Data.Time (diffUTCTime, getCurrentTime)

-- test-framework
import Test.Framework (testGroup)

-- test-framework-hunit
import Test.Framework.Providers.HUnit (testCase)

-- test-framework-QuickCheck2
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- QuickCheck2
import Test.QuickCheck (Arbitrary, genericShrink, ioProperty, (.&&.), (===))
import Test.QuickCheck.Arbitrary.Generic (Arbitrary (..), genericArbitrary)

-- hunit
import Test.HUnit ((@?=))
import Test.HUnit.Base (assert)

-- monad-schedule
import Control.Monad.Schedule.Class (async, scheduleAndFinish)
import Control.Monad.Schedule.FreeAsync (FreeAsync, asyncMVar, concurrently, freeAsync, runConcurrently, runFreeAsync)

tests =
  testGroup
    "FreeAsync"
    [ testCase "Can lift and run an IORef action" $ do
        ref <- newIORef ""
        runFreeAsync $ liftIO $ writeIORef ref "Hello"
        result <- readIORef ref
        result @?= "Hello"
    , testProperty "Can schedule two values" $ \(v1 :: Value) (v2 :: Value) -> ioProperty $ do
        (result1, result2) <- runFreeAsync $ async (interpretValue 23 v1) (interpretValue 42 v2)
        return $ result1 === 23 .&&. result2 === 42
    , testCase "Can schedule two MVars" $ do
        var1 <- newMVar 23
        var2 <- newMVar 42
        result <- runFreeAsync $ async (asyncMVar var1) (asyncMVar var2)
        result @?= (23, 42)
    , testCase "Can schedule lifted retrieval of two MVars" $ do
        var1 <- newMVar 23
        var2 <- newMVar 42
        result <- runFreeAsync $ async (liftIO $ takeMVar var1) (liftIO $ takeMVar var2)
        result @?= (23, 42)
    , testCase "Sequencing adds times" $ do
        before <- getCurrentTime
        runFreeAsync $ replicateM_ 100 $ freeAsync $ threadDelay 1000
        after <- getCurrentTime
        let diff = after `diffUTCTime` before
        print diff
        assert $ diff >= 0.1 && diff < 0.3
    , testCase "Scheduling does things in parallel" $ do
        before <- getCurrentTime
        runFreeAsync $ scheduleAndFinish $ fromList $ replicate 100 $ freeAsync $ threadDelay 1000
        after <- getCurrentTime
        let diff = after `diffUTCTime` before
        print diff
        assert $ diff > 0.001 && diff <= 0.01 -- Assume overhead is at most 10x at ms durations
    , testGroup
        "ConcurrentlyT"
        [ testCase "Sequencing does things in parallel" $ do
            before <- getCurrentTime
            runConcurrently $ replicateM_ 100 $ concurrently $ threadDelay 1000
            after <- getCurrentTime
            let diff = after `diffUTCTime` before
            print diff
            assert $ diff > 0.001 && diff <= 0.01 -- Assume overhead is at most 10x at ms durations
        ]
    ]

data Value
  = Pure
  | Lifted
  | FreeAsync_
  | AsyncMVar
  | TakeMVar
  | TakeMVarSingleLift
  deriving (Show, Generic)

instance Arbitrary Value where
  arbitrary = genericArbitrary
  shrink = genericShrink

interpretValue :: a -> Value -> FreeAsync a
interpretValue a Pure = pure a
interpretValue a Lifted = liftIO (pure a)
interpretValue a FreeAsync_ = freeAsync $ pure a
interpretValue a AsyncMVar = liftIO (newMVar a) >>= asyncMVar
interpretValue a TakeMVar = liftIO (newMVar a) >>= liftIO . takeMVar
interpretValue a TakeMVarSingleLift = liftIO (newMVar a >>= takeMVar)
