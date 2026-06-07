module Automaton.Schedule where

-- base
import Control.Category ((>>>))
import Control.Concurrent (threadDelay, yield)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (runIdentity))
import Data.Foldable (Foldable (..))
import Data.Functor (($>))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Sum (..))

-- quickcheck
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run)

-- transformers
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer.Strict (WriterT (..), tell)

-- mmorph
import Control.Monad.Morph (MFunctor (..))

-- changeset
import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Trans.Changeset (Count (Increment), changeSingle, runChangeset)

-- tasty
import Test.Tasty (TestTree, testGroup)

-- tasty-quickcheck
import Test.Tasty.QuickCheck (testProperty)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Data.Automaton (Automaton, accumulateWith, arrM, constM, embed, hoistS)
import Data.Automaton qualified as Automaton
import Data.Automaton.Schedule (FinalizeT (..), MonadSchedule, schedule)
import Data.Automaton.Schedule.Trans (ScheduleT, runScheduleT, runSkipS, runYield, skip, wait)
import Data.Automaton.Trans.Maybe (runMaybeS)
import Data.Stream.Result (Result (..))
import Data.TimeDomain (Seconds (..))

tests =
  testGroup
    "Schedule"
    [ testGroup
        "FinalizeT"
        [ testCase "FinalizeT stops when the single automaton completes" $ do
            -- A single automaton counting down from 3 should produce 3 outputs then stop.
            -- We lower to Identity via runMaybeS so outputs are not discarded on termination.
            let outputs = embedFinalize (schedule $ pure (make 3))
            length outputs @?= 3
        , testCase "FinalizeT waits for all automata to complete" $ do
            -- Two automata: A counts down from 2, B from 3.
            -- Identity schedule runs both in lockstep; outputs are interleaved then flattened.
            -- Total outputs: 2 + 3 = 5
            let outputs = embedFinalize (schedule $ make 2 :| [make 3])
            length outputs @?= 5
        , testCase "FinalizeT does not stop when the first automaton completes" $ do
            -- Unlike MaybeT, FinalizeT keeps running while any automaton is still alive.
            -- A (2 steps) finishes first; B (3 steps) finishes last.
            let maybeOutputs = embedMaybe (schedule $ makeMaybe 2 :| [makeMaybe 3])
                finalizeOutputs = embedFinalize (schedule $ make 2 :| [make 3])
            -- MaybeT stops when A finishes: both emit 2 outputs before MaybeT short-circuits
            length maybeOutputs @?= 4
            -- FinalizeT keeps going until B also finishes: 2 + 3 = 5 outputs
            length finalizeOutputs @?= 5
        , testCase "FinalizeT round-robin output order" $ do
            -- Two automata: A emits 10, 20 then stops; B emits 100, 200, 300 then stops.
            -- Identity schedule steps both simultaneously, then concatS flattens:
            --   tick 1: [10, 100], tick 2: [20, 200], tick 3: [300] (A done, B still live)
            -- Outputs: [10, 100, 20, 200, 300]
            let mkCounting startVal stepVal n =
                  Automaton.unfoldM (0 :: Int) $ const $ \k ->
                    if k >= n
                      then FinalizeT $ MaybeT $ pure Nothing
                      else FinalizeT $ MaybeT $ pure $ Just $ Result (k + 1) (startVal + stepVal * k :: Int)
                outputs = embedFinalize (schedule $ mkCounting 10 10 2 :| [mkCounting 100 100 3])
            outputs @?= [10, 100, 20, 200, 300]
        ]
    , testGroup
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
    , scheduleTests
        "ScheduleT IO busy"
        id
        (const yield)
    , scheduleTests
        "ScheduleT IO with delay"
        id
        (liftIO . threadDelay . (* 10) . fromIntegral)
    , scheduleTests
        "ScheduleT Identity"
        (pure . runIdentity)
        (const $ pure ())
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
    , testGroup
        "MaybeT"
        [ testCase "MaybeT stops all automata when one returns Nothing" $ do
            -- Two automata: one finite (stops after 3 steps), one infinite.
            -- When the finite one stops, the whole scheduled automaton stops too.
            let finite =
                  Automaton.unfoldM (3 :: Int) $ const $ \n ->
                    if n <= 0
                      then MaybeT $ pure Nothing
                      else pure $ Result (n - 1) n
                infinite = constM $ pure (0 :: Int)
                output = runIdentity $ embed (runMaybeS (schedule $ finite :| [infinite])) $ replicate 10 ()
            -- Stops as soon as finite returns Nothing
            output @?= [Just 3, Just 0, Just 2, Just 0, Just 1, Just 0, Nothing, Nothing, Nothing, Nothing]
        , testCase "MaybeT all-Nothing immediately gives Nothing" $ do
            let alwaysNothing = constM (MaybeT $ pure Nothing) :: Automaton (MaybeT Identity) () Int
                output = runIdentity $ embed (runMaybeS (schedule $ alwaysNothing :| [alwaysNothing])) (replicate 5 ())
            output @?= [Nothing, Nothing, Nothing, Nothing, Nothing]
        ]
    , testGroup
        "ExceptT"
        [ testCase "ExceptT stops all automata when one throws" $ do
            -- One automaton throws after 3 steps, the other never throws.
            let throwing =
                  Automaton.unfoldM (3 :: Int) $ const $ \n ->
                    if n <= 0
                      then throwE ("done" :: String)
                      else pure $ Result (n - 1) n
                nonthrowing = constM $ pure (0 :: Int)
                output = runIdentity $ runExceptT $ embed (schedule $ throwing :| [nonthrowing]) $ replicate 10 ()
            -- Should terminate with Left, not run all 10 steps
            case output of
              Left _ -> pure ()
              Right xs -> fail $ "Expected Left but got Right with " <> (show (length xs) <> " elements")
        ]
    ]
  where
    waitSkip n = replicateM (n - 1) skip
    -- \| Automaton in FinalizeT that counts down from n, outputting each value, then terminates.
    make :: Int -> Automaton (FinalizeT Identity) () Int
    make n = Automaton.unfoldM n $ const $ \k ->
      if k <= 0
        then FinalizeT $ MaybeT $ pure Nothing
        else FinalizeT $ MaybeT $ pure $ Just $ Result (k - 1) k
    -- \| Automaton in MaybeT that counts down from n, outputting each value, then terminates.
    makeMaybe :: Int -> Automaton (MaybeT Identity) () Int
    makeMaybe n = Automaton.unfoldM n $ const $ \k ->
      if k <= 0
        then MaybeT $ pure Nothing
        else MaybeT $ pure $ Just $ Result (k - 1) k
    -- \| Run a FinalizeT automaton and collect all outputs produced before it terminates.
    -- Uses runMaybeS to lower to Identity, so the outputs are not discarded on termination.
    embedFinalize :: Automaton (FinalizeT Identity) () b -> [b]
    embedFinalize automaton =
      fmap fromJust
        . takeWhile isJust
        . runIdentity
        $ embed (runMaybeS $ hoistS getFinalizeT automaton) (replicate 100 ())
    -- \| Run a MaybeT automaton and collect all outputs produced before it terminates.
    embedMaybe :: Automaton (MaybeT Identity) () b -> [b]
    embedMaybe automaton =
      fmap fromJust
        . takeWhile isJust
        . runIdentity
        $ embed (runMaybeS automaton) (replicate 100 ())

scheduleTests ::
  (Monad m, MonadSchedule m) =>
  String ->
  (forall a. m a -> IO a) ->
  (Seconds Integer -> m ()) ->
  TestTree
scheduleTests name runProperty interpretSchedule =
  testGroup
    name
    [ testCase "schedule waits chronologically" $ do
        output <- runProperty $ runScheduleT interpretSchedule $ embed (schedule $ mkClock <$> 3 :| [5]) $ replicate 10 ()
        output @?= [3, 5, 6, 9, 10, 12, 15, 15, 18, 20]
    , testCase "schedule chronologically (mirrored)" $ do
        output <- runProperty $ runScheduleT interpretSchedule $ embed (schedule $ mkClock <$> 5 :| [3]) $ replicate 10 ()
        output @?= [3, 5, 6, 9, 10, 12, 15, 15, 18, 20]
    , testProperty "multiple automata schedule correctly" $ within 1_000_000 $ \(diffss :: NonEmpty Diffs) -> monadicIO $ do
        output <- fmap (List.nub . snd) $ run $ runProperty $ runScheduleT interpretSchedule $ runWriterT $ runMaybeT $ embed (hoistS (hoist lift) (schedule $ runningClock <$> diffss) >>> arrM (lift . tell . pure)) $ repeat ()
        pure $ output === take (length output) (List.nub $ List.sort $ concatMap accDiffs diffss)
    ]
  where
    mkClock n = constM (wait n $> n) >>> accumulateWith (+) (0 :: Seconds Integer)

-- These instances are needed because 'time-domain' does not yet provide
-- 'Semigroup'/'Monoid' for 'Integer'. Remove once time-domain is updated.
deriving via (Sum Integer) instance Semigroup Integer

deriving via (Sum Integer) instance Monoid Integer

type Diffs = [Positive (Seconds Integer)]

type Times = [Seconds Integer]

accDiffs :: Diffs -> Times
accDiffs = drop 1 . scanl (\t (Positive dt) -> t + dt) 0 . toList

interpretDiffs :: (Monad m) => Diffs -> Automaton (MaybeT (ScheduleT (Seconds Integer) m)) () (Seconds Integer)
interpretDiffs diffs0 = Automaton.unfoldM (toList diffs0) $ const $ \case
  [] -> MaybeT $ pure Nothing
  (Positive diff : diffs) -> lift (wait diff) >> pure (Result diffs diff)

runningClock :: (Monad m) => Diffs -> Automaton (MaybeT (ScheduleT (Seconds Integer) m)) () (Seconds Integer)
runningClock diffs = interpretDiffs diffs >>> accumulateWith (+) 0

-- 'NonEmpty' has no 'Arbitrary' instance in QuickCheck as of 2.14.
instance (Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Seconds a) where
  arbitrary = Seconds <$> arbitrary
