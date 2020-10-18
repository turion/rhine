{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
-- base
import Control.Monad
import Data.Functor.Identity
import Data.List.NonEmpty

-- rhine
import Control.Applicative
import Control.Monad.Schedule
import Control.Monad.Schedule.Class

-- free
import Control.Monad.Trans.Free

-- test-framework
import Test.Framework

-- test-framework-hunit
import Test.Framework.Providers.HUnit

-- HUnit
import Test.HUnit hiding (Test)
import Control.Arrow

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "MonadSchedule"
    [ testCase "Scheduling two waits" $ assertEqual "Yep" (essence lhs) (essence rhs)
    , testProperty "Initial order doesn't matter"
      $ forAll (scriptsAndShuffled @String) $ \(script, shuffled)
        -> snd (essence' (play <$> script)) === snd (essence' (play <$> shuffled))
    -- , testProperty "Initial order doesn't matter" $ forAll ((essence' *** essence') <$> schedulesAndShuffled @String) $ uncurry (===)
    -- TODO Find a cooler property, maybe with Writer
    ]
  ]

type Schedule a = ScheduleT Integer Identity a

data Script a = Script a [Integer]
  deriving Show

instance Arbitrary a => Arbitrary (Script a) where
  arbitrary = Script <$> arbitrary <*> listOf (choose (1, 1000))

play :: Script a -> Schedule a
play (Script a waits) = forM_ waits wait >> return a

-- TODO Alternative instance for Gen
instance Arbitrary a => Arbitrary (Schedule a) where
  arbitrary = play <$> arbitrary

scriptsAndShuffled :: Arbitrary a => Gen (NonEmpty (Script a), NonEmpty (Script a))
scriptsAndShuffled = do
  scripts <- listOf1 arbitrary
  shuffled <- shuffle scripts
  return (fromList scripts, fromList shuffled)

schedulesAndShuffled :: Arbitrary a => Gen (NonEmpty (Schedule a), NonEmpty (Schedule a))
schedulesAndShuffled = do
  schedules <- listOf1 arbitrary
  shuffled <- shuffle schedules
  return (fromList schedules, fromList shuffled)

lhs :: NonEmpty (Schedule ())
lhs = fromList [wait 23, wait 42]
rhs :: NonEmpty (Schedule ())
rhs = fromList [wait 42, wait 23]

essence :: NonEmpty (Schedule ()) -> ([[Integer]], [Integer])
essence = schedule
  >>> execScheduleT
  >>> runIdentity
  >>> first (snd >>> fmap (execScheduleT >>> runIdentity >>> snd))
-- essence = fmap (runIdentity . execScheduleT) . snd . runIdentity . execScheduleT . schedule

essence' :: NonEmpty (Schedule a) -> (a, [Integer])
essence' = schedule
  >>> execScheduleT
  >>> runIdentity
  >>> first fst
