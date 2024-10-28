{-# LANGUAGE OverloadedLists #-}

module Schedule where

-- base
import Control.Arrow (arr, (>>>))
import Data.Functor (($>))
import Data.Functor.Identity
import Data.List (sort)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit

-- monad-schedule
import Control.Monad.Schedule.Trans (Schedule, ScheduleT, runScheduleIO, runScheduleT, wait)

-- automaton
import Data.Automaton (accumulateWith, arrM, constM, embed, sumN, Automaton)

-- rhine

import Control.Concurrent (threadDelay)
import Control.Monad.Schedule.FreeAsync (FreeAsync, FreeAsyncT (FreeAsyncT), runFreeAsync)
import Data.List.NonEmpty (toList)
import FRP.Rhine.Clock (Clock (initClock), RunningClockInit)
import FRP.Rhine.Clock.FixedStep (FixedStep (FixedStep))
import FRP.Rhine.Schedule
import Util
import Control.Monad.IO.Class (MonadIO(..))

tests =
  testGroup
    "Schedule"
    [ testGroup
        "scheduleList"
        [ testCase "schedule waits chronologically" $ do
            let output = runIdentity $ runScheduleT (const (pure ())) $ embed (scheduleList $ (\n -> constM (wait n $> n) >>> accumulateWith (+) 0) <$> [3 :: Integer, 5]) $ replicate 6 ()
            output @?= pure <$> [3, 5, 6, 9, 10, 12]
        , testCase "schedule waits chronologically (mirrored)" $ do
            let output = runSchedule $ embed (scheduleList $ (\n -> constM (wait n $> n) >>> accumulateWith (+) 0) <$> [5 :: Integer, 3]) $ replicate 6 ()
            output @?= pure <$> [3, 5, 6, 9, 10, 12]
        ]
    , testGroup
        "runningSchedule"
        [ testCase "chronological ticks" $ do
            let clA = FixedStep @5
                clB = FixedStep @3
                (runningClockA, _) = runSchedule (initClock clA :: RunningClockInit (Schedule Integer) Integer ())
                (runningClockB, _) = runSchedule (initClock clB :: RunningClockInit (Schedule Integer) Integer ())
                output = runSchedule $ embed (runningSchedule clA clB runningClockA runningClockB) $ replicate 6 ()
            output
              @?= [ (3, Right ())
                  , (5, Left ())
                  , (6, Right ())
                  , (9, Right ())
                  , (10, Left ())
                  , (12, Right ())
                  ]
        ]
    , testGroup
        "ParallelClock ScheduleT IO"
        [ testCase "chronological ticks" $ do
            (runningClock, _time) <- runScheduleIO (initClock $ ParallelClock (FixedStep @500) (FixedStep @300) :: RunningClockInit (ScheduleT Integer IO) Integer (Either () ()))
            output <- runScheduleIO $ embed runningClock $ replicate 20 ()
            take 6 output
              @?= [ (300, Right ())
                  , (500, Left ())
                  , (600, Right ())
                  , (900, Right ())
                  , (1000, Left ())
                  , (1200, Right ())
                  ]
            let timestamps = fst <$> output
            timestamps @?= sort timestamps
        ]
    , testGroup
        "ParallelClock FreeAsync"
        [ testCase "chronological ticks" $ do
            (runningClock, _time) <- runFreeAsync $ runScheduleIO (initClock $ ParallelClock (FixedStep @500) (FixedStep @300) :: RunningClockInit (ScheduleT Integer (FreeAsync)) Integer (Either () ()))
            output <- runFreeAsync $ runScheduleIO $ embed runningClock $ replicate 20 ()
            take 6 output
              @?= [ (300, Right ())
                  , (500, Left ())
                  , (600, Right ())
                  , (900, Right ())
                  , (1000, Left ())
                  , (1200, Right ())
                  ]
            let timestamps = fst <$> output
            timestamps @?= sort timestamps
        ]
    , testGroup
        "automaton"
        [ testCase "IO" $ do
            let automatonN n = constM (threadDelay $ n * 100000) >>> arr (const n) >>> sumN
            output <- embed (scheduleList [automatonN 3, automatonN 5]) (replicate 20 ())
            let timestamps = concatMap toList output
            timestamps @?= sort timestamps
        , testCase "ScheduleT IO without formal action" $ do
            let automatonN n = (constM (liftIO $ threadDelay $ n * 100000) >>> arr (const n) >>> sumN) :: Automaton (ScheduleT Integer IO) () Int
            output <- runScheduleIO $ embed (scheduleList [automatonN 3, automatonN 5]) (replicate 20 ())
            let timestamps = concatMap toList output
            timestamps @?= sort timestamps
        , testCase "ScheduleT IO with formal action" $ do
            let automatonN n = (constM (wait $ n * 100) >>> arr (const n) >>> sumN) :: Automaton (ScheduleT Integer IO) () Integer
            output <- runScheduleIO $ embed (scheduleList [automatonN 3, automatonN 5]) (replicate 20 ())
            let timestamps = concatMap toList output
            timestamps @?= sort timestamps
        ]
    ]
