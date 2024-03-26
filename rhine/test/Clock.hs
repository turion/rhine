module Clock where

-- tasty
import Test.Tasty

-- rhine
<<<<<<< HEAD
=======
import Clock.Catch
>>>>>>> f7d003c (WIP)
import Clock.Except
import Clock.FixedStep
import Clock.Millisecond

tests =
  testGroup
    "Clock"
<<<<<<< HEAD
    [ Clock.Except.tests
=======
    [ Clock.Catch.tests
    , Clock.Except.tests
>>>>>>> f7d003c (WIP)
    , Clock.FixedStep.tests
    , Clock.Millisecond.tests
    ]
