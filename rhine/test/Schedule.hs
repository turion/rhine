{-# LANGUAGE OverloadedLists #-}

module Schedule where

-- base
import Control.Monad (replicateM)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit

-- automaton
import Data.Automaton (embed)
import Data.Automaton.Schedule (Yield, runYield, skip)

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
                (runningClockA, _) = runYield (initClock clA :: RunningClockInit Yield Integer ())
                (runningClockB, _) = runYield (initClock clB :: RunningClockInit Yield Integer ())
                output = runYield $ embed (runningSchedule clA clB runningClockA runningClockB) $ replicate 6 ()
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
              (runningClock, _time) = runYield (initClock (ParallelClock (FixedStep @5) (FixedStep @3)) :: RunningClockInit Yield Integer (Either () ()))
              output = runYield $ embed runningClock $ replicate 6 ()
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
