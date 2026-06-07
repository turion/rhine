{-# LANGUAGE OverloadedLists #-}

module Schedule where

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit

-- time-domain
import Data.TimeDomain (Seconds)

-- automaton
import Data.Automaton (embed)
import Data.Automaton.Schedule.Trans (Schedule, evalSchedule)

-- rhine
import FRP.Rhine (FixedStep (..), ParallelClock (..), initClock, runningSchedule)
import FRP.Rhine.Clock (RunningClockInit)

tests =
  testGroup
    "Schedule"
    [ testGroup
        "scheduling running clocks"
        [ testCase "chronological ticks" $ do
            let clA = FixedStep @5
                clB = FixedStep @3
                (runningClockA, _) = evalSchedule (initClock clA :: RunningClockInit (Schedule (Seconds Integer)) (Seconds Integer) ())
                (runningClockB, _) = evalSchedule (initClock clB :: RunningClockInit (Schedule (Seconds Integer)) (Seconds Integer) ())
                output = evalSchedule $ embed (runningSchedule clA clB runningClockA runningClockB) $ replicate 6 ()
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
            let (runningClock, _time) = evalSchedule (initClock (ParallelClock (FixedStep @5) (FixedStep @3)) :: RunningClockInit (Schedule (Seconds Integer)) (Seconds Integer) (Either () ()))
                output = evalSchedule $ embed runningClock $ replicate 6 ()
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
