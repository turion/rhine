module Clock where

-- tasty
import Test.Tasty

-- rhine
import Clock.FixedStep
import Clock.Millisecond

tests =
  testGroup
    "Clock"
    [ Clock.FixedStep.tests
    , Clock.Millisecond.tests
    ]
