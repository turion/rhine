module Clock.Millisecond where

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- rhine
import FRP.Rhine
import Util (runRhine)

secondsSinceInit :: (Monad m) => ClSF m (Millisecond n) a Int
secondsSinceInit = sinceInitS >>> arr round

tests =
  testGroup
    "Millisecond"
    [ testCase "Runs to second precision" $ do
        output <- runRhine (secondsSinceInit @@ (waitClock @1000)) $ replicate 5 ()
        output @?= Just <$> [1, 2, 3, 4, 5]
    , testCase "Schedules chronologically" $ do
        output <- runRhine (secondsSinceInit @@ (waitClock @3000) >-- collect --> (clId &&& secondsSinceInit) @@ (waitClock @5000)) $ replicate 5 ()
        output
          @?= [ Nothing
              , Just ([3], 5)
              , Nothing
              , Nothing
              , Just ([9, 6], 10)
              ]
    ]
