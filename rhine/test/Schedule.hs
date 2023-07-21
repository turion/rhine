{-# LANGUAGE OverloadedLists #-}

module Schedule where

-- base
import Control.Arrow ((>>>))
import Data.Functor (($>))
import Data.Functor.Identity

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit

-- monad-schedule
import Control.Monad.Schedule.Trans (Schedule, runScheduleT, wait)

-- rhine
import FRP.Rhine.Clock (Clock (initClock), RunningClockInit, accumulateWith, constM, embed)
import FRP.Rhine.Clock.FixedStep (FixedStep (FixedStep))
import FRP.Rhine.Schedule
import Util

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
        "ParallelClock"
        [ testCase "chronological ticks" $ do
            let
              (runningClock, _time) = runSchedule (initClock $ ParallelClock (FixedStep @5) (FixedStep @3) :: RunningClockInit (Schedule Integer) Integer (Either () ()))
              output = runSchedule $ embed runningClock $ replicate 6 ()
            output
              @?= [ (3, Right ())
                  , (5, Left ())
                  , (6, Right ())
                  , (9, Right ())
                  , (10, Left ())
                  , (12, Right ())
                  ]
        ]
    ]
