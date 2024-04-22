{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clock.Millisecond where

-- base
import Control.Monad (when)
import System.Info (os)

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

-- rhine
import FRP.Rhine
import Util (runRhine)

-- | Milliseconds
newtype MS = MS Int
  deriving (Num, Show, Eq, Ord)

millisecondsSinceInit :: (Monad m) => ClSF m (Millisecond n) a MS
millisecondsSinceInit = sinceInitS >>> arr (MS . round . (* 1000))

tests =
  testGroup
    "Millisecond"
    [ testCase "Outputs milliseconds chronologically" $ do
        output <- runRhine (millisecondsSinceInit @@ (waitClock @1)) $ replicate 5 ()
        assertTiming output $ Just <$> [1, 2, 3, 4, 5]
    , testCase "Schedules chronologically" $ do
        output <- runRhine (millisecondsSinceInit @@ (waitClock @30) >-- collect --> (clId &&& millisecondsSinceInit) @@ (waitClock @50)) $ replicate 5 ()
        assertTiming
          output
          [ Nothing
          , Just ([30], 50)
          , Nothing
          , Nothing
          , Just ([90, 60], 100)
          ]
    ]

assertTiming :: (Show a, TimingSubsumes a) => a -> a -> Assertion
assertTiming observed expected =
  when (os /= "darwin") $
    assertBool ("Observed timing: " ++ show observed ++ "\nExpected timing: " ++ show expected) $
      timingSubsumes observed expected

class TimingSubsumes a where
  timingSubsumes :: a -> a -> Bool

instance TimingSubsumes MS where
  timingSubsumes tObserved tExpected = tExpected <= tObserved && tObserved <= 2 * tExpected + 10

instance (TimingSubsumes a) => TimingSubsumes (Maybe a) where
  timingSubsumes (Just aObserved) (Just aExpected) = timingSubsumes aObserved aExpected
  timingSubsumes Nothing Nothing = True
  timingSubsumes _ _ = False

instance (TimingSubsumes a, TimingSubsumes b) => TimingSubsumes (a, b) where
  timingSubsumes (aObserved, bObserved) (aExpected, bExpected) = timingSubsumes aObserved aExpected && timingSubsumes bObserved bExpected

instance (TimingSubsumes a) => TimingSubsumes [a] where
  timingSubsumes [] [] = True
  timingSubsumes (aObserved : aObserveds) (aExpected : aExpecteds) = timingSubsumes aObserved aExpected && timingSubsumes aObserveds aExpecteds
  timingSubsumes _ _ = False
