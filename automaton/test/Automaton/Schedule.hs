module Automaton.Schedule where

-- base
import Control.Category ((>>>))
import Control.Monad (replicateM)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))

-- changeset

import Control.Monad.Changeset.Class (MonadChangeset (..))
import Control.Monad.Trans.Changeset (Count (Increment), changeSingle, runChangeset)

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Data.Automaton (accumulateWith, constM, embed)
import Data.Automaton.Schedule (runSkipS, runYield, schedule, skip)

tests =
  testGroup
    "Schedule"
    [ testGroup
        "SkipT"
        [ testCase "SkipT skips an output step" $ do
            let output = runIdentity $ embed (runSkipS $ constM (wait 5 $> (5 :: Int)) >>> accumulateWith (+) 0) $ replicate 10 ()
            output @?= [Nothing, Nothing, Nothing, Nothing, Just 5, Nothing, Nothing, Nothing, Nothing, Just 10]
        , testCase "schedule waits chronologically (mirrored)" $ do
            let output = runIdentity $ embed (runSkipS $ constM (wait 3 $> (3 :: Int)) >>> accumulateWith (+) 0) $ replicate 10 ()
            output @?= [Nothing, Nothing, Just 3, Nothing, Nothing, Just 6, Nothing, Nothing, Just 9, Nothing]
        ]
    , testGroup
        "schedule"
        [ testCase "schedule waits chronologically" $ do
            let output = runYield $ embed (schedule $ (\n -> constM (wait n $> n) >>> accumulateWith (+) 0) <$> 3 :| [5]) $ replicate 10 ()
            output @?= [3, 5, 6, 9, 10, 12, 15, 15, 18, 20]
        , testCase "schedule waits chronologically (mirrored)" $ do
            let output = runYield $ embed (schedule $ (\n -> constM (wait n $> n) >>> accumulateWith (+) 0) <$> 5 :| [3]) $ replicate 10 ()
            output @?= [3, 5, 6, 9, 10, 12, 15, 15, 18, 20]
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
    wait n = replicateM (n - 1) skip
