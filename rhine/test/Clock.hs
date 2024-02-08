module Clock where

-- tasty
import Test.Tasty

-- rhine
import Clock.Except
import Clock.FixedStep
import Clock.Millisecond

tests =
  testGroup
    "Clock"
    [ Clock.Except.tests
    , Clock.FixedStep.tests
    , Clock.Millisecond.tests
    ]
