module Clock.Millisecond where

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- rhine
import FRP.Rhine
import Util (runRhine)

msSinceInit :: Monad m => ClSF m (Millisecond n) a Int
msSinceInit = sinceInitS >>> arr (\seconds -> round $ seconds * 1000)

tests =
  testGroup
    "Millisecond"
    [ testCase "Runs to millisecond precision" $ do
        output <- runRhine (msSinceInit @@ (waitClock @1)) $ replicate 5 ()
        output @?= Just <$> [1, 2, 3, 4, 5]
    , testCase "Schedules chronologically" $ do
        output <- runRhine (msSinceInit @@ (waitClock @3) >-- collect --> (clId &&& msSinceInit) @@ (waitClock @5)) $ replicate 5 ()
        output
          @?= [ Nothing
              , Just ([3], 5)
              , Nothing
              , Nothing
              , Just ([9, 6], 10)
              ]
    ]
