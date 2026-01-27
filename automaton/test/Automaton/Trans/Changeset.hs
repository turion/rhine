module Automaton.Trans.Changeset where

-- base
import Control.Monad.Identity (Identity (runIdentity))
import Data.Monoid (Sum (..))

-- transformers
import Control.Monad.Changeset.Class (change, current)

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Data.Automaton
import Data.Automaton.Trans.Changeset (runChangesetS)

tests = testGroup "Trans.Changeset" [testCase "runChangesetS" $ runIdentity (embed (runChangesetS (Sum (0 :: Int)) (constM (change (Sum (1 :: Int)) >> current))) (replicate 5 ())) @?= (\n -> (n, n)) <$> [1, 2, 3, 4, 5]]
