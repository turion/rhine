{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
module Automaton.Schedule where

-- base
import Control.Category ((>>>))
import Control.Monad (replicateM)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Sum(..))
import Control.Concurrent (yield)
import Data.Foldable (Foldable(..))
import qualified Data.List as List

-- quickcheck
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.QuickCheck

-- transformers
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Writer.Strict (WriterT(..), tell)

-- mmorph
import Control.Monad.Morph (MFunctor(..))

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Trans.Changeset (Count (Increment), changeSingle, runChangeset)

-- tasty
import Test.Tasty (testGroup)

-- tasty-quickcheck
import Test.Tasty.QuickCheck (testProperty)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- monad-schedule
import Control.Monad.Schedule.Trans (ScheduleT, wait, runScheduleIO, runScheduleT)

-- automaton
import Data.Automaton (accumulateWith, constM, embed, arrM, hoistS)
import Data.Automaton.Schedule (runSkipS, runYield, schedule, skip)
import Data.Automaton (Automaton)
import qualified Data.Automaton as Automaton
import Data.Stream.Result (Result(..))

tests =
  testGroup
    "Schedule"
    [ testGroup
        "SkipT"
        [ testCase "SkipT skips an output step" $ do
            let output = runIdentity $ embed (runSkipS $ constM (waitSkip 5 $> (5 :: Int)) >>> accumulateWith (+) 0) $ replicate 10 ()
            output @?= [Nothing, Nothing, Nothing, Nothing, Just 5, Nothing, Nothing, Nothing, Nothing, Just 10]
        , testCase "schedule waits chronologically (mirrored)" $ do
            let output = runIdentity $ embed (runSkipS $ constM (waitSkip 3 $> (3 :: Int)) >>> accumulateWith (+) 0) $ replicate 10 ()
            output @?= [Nothing, Nothing, Just 3, Nothing, Nothing, Just 6, Nothing, Nothing, Just 9, Nothing]
        ]
    , testGroup
        "Yield"
        [ testCase "schedule waits chronologically" $ do
            let output = runYield $ embed (schedule $ (\n -> constM (waitSkip n $> n) >>> accumulateWith (+) 0) <$> 3 :| [5]) $ replicate 10 ()
            output @?= [3, 5, 6, 9, 10, 12, 15, 15, 18, 20]
        , testCase "schedule waits chronologically (mirrored)" $ do
            let output = runYield $ embed (schedule $ (\n -> constM (waitSkip n $> n) >>> accumulateWith (+) 0) <$> 5 :| [3]) $ replicate 10 ()
            output @?= [3, 5, 6, 9, 10, 12, 15, 15, 18, 20]
        ]
    , testGroup
        "ScheduleT IO"
        [ testCase "schedule waits chronologically" $ do
            output <- runScheduleIO $ embed (schedule $ (\n -> constM (wait n $> n) >>> accumulateWith (+) (0 :: Integer)) <$> 3 :| [5]) $ replicate 10 ()
            output @?= [3, 5, 6, 9, 10, 12, 15, 15, 18, 20]
        , testCase "schedule chronologically (mirrored)" $ do
            output <- runScheduleIO $ embed (schedule $ (\n -> constM (wait n $> n) >>> accumulateWith (+) (0 :: Integer)) <$> 5 :| [3]) $ replicate 10 ()
            output @?= [3, 5, 6, 9, 10, 12, 15, 15, 18, 20]
        , testProperty "multiple automata schedule correctly" $ within 1_000_000 $ \(diffss :: NonEmpty Diffs) -> monadicIO $ do
            output <- fmap (List.nub . snd) $ run $ runScheduleT (const yield) $ runWriterT $ runMaybeT $ embed (hoistS (hoist lift) (schedule $ runningClock <$> diffss) >>> (arrM $ lift . tell . pure)) $ repeat ()
            pure $ output === (take (length output) $ List.nub $ List.sort $ concat $ accDiffs <$> diffss)
        ]
    , testGroup
        "ChangesetT"
        [ testCase "Single automaton is unchanged" $ do
            let output = flip runChangeset (0 :: Int) $ flip embed (replicate 5 ()) $ schedule $ pure $ constM $ changeSingle Increment >> current
            output @?= ([1, 2, 3, 4, 5], 5)
        , testCase "Two automata see global state" $ do
            let output = flip runChangeset (0 :: Int) $ flip embed (replicate 10 ()) $ schedule $ constM (changeSingle Increment >> pure (-1)) :| [constM current]
            output
              @?= (
                    [ -1
                    , 0 -- First tick of both automata: Second one doesn't yet see the log of the other
                    , -1
                    , 1 -- Second joint tick: Log from the first reaches the second automaton
                    , -1
                    , 2
                    , -1
                    , 3
                    , -1
                    , 4
                    ]
                  , 5
                  )
        , testCase "Two automata see global state (mirrored)" $ do
            let output = flip runChangeset (0 :: Int) $ flip embed (replicate 10 ()) $ schedule $ constM current :| [constM (changeSingle Increment >> pure (-1))]
            output
              @?= (
                    [ 0 -- First tick of both automata: Second one doesn't yet see the log of the other
                    , -1
                    , 1 -- Second joint tick: Log from the first reaches the second automaton
                    , -1
                    , 2
                    , -1
                    , 3
                    , -1
                    , 4
                    , -1
                    ]
                  , 5
                  )
        ]
    ]
  where
    waitSkip n = replicateM (n - 1) skip

-- FIXME shouldn't need these, update time-domain first
deriving via (Sum Integer) instance Semigroup Integer
deriving via (Sum Integer) instance Monoid Integer

-- FIXME temporarily only test nonempty lists
type Diffs = NonEmpty (Positive Integer)
-- type Diffs = [Positive Integer]
type Times = [Integer]

accDiffs :: Diffs -> Times
accDiffs = drop 1 . scanl (\t (Positive dt) -> t + dt) 0 . toList

interpretDiffs :: Diffs -> Automaton (MaybeT (ScheduleT Integer IO)) () Integer
interpretDiffs diffs0 = Automaton.unfoldM (toList diffs0) $ const $ \case
  [] -> MaybeT $ pure Nothing
  (Positive diff : diffs) -> lift (wait diff) >> pure (Result diffs diff)

runningClock :: Diffs -> Automaton (MaybeT (ScheduleT Integer IO)) () Integer
runningClock diffs = interpretDiffs diffs >>> accumulateWith (+) 0

-- FIXME Why is this not in QuickCheck?
instance (Arbitrary a) => Arbitrary (NonEmpty a) where
  -- arbitrary = (:|) <$> arbitrary <*> arbitrary
  -- FIXME Temporarily only test lists of length at least 2
  arbitrary = (:|) <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary)
