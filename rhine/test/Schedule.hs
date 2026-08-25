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
import Data.Automaton.Schedule.Trans (evalSchedule)

-- rhine
import FRP.Rhine (FixedStep (..), ParallelClock (..), runClock)

tests =
  testGroup
    "Schedule"
    [ testGroup
        "scheduling running clocks"
        [ testCase "chronological ticks" $ do
            let clA = FixedStep @5
                clB = FixedStep @3
                output = evalSchedule @(Seconds Integer) $ embed runClock $ replicate 6 $ ParallelClock clA clB
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
            let output = evalSchedule @(Seconds Integer) $ embed runClock $ replicate 6 $ ParallelClock (FixedStep @5) (FixedStep @3)
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
