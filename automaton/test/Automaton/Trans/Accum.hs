module Automaton.Trans.Accum where

-- base
import Control.Monad.Identity (Identity (runIdentity))
import Data.Monoid (Sum (..))

-- transformers
import Control.Monad.Trans.Accum (add, look)

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Data.Automaton
import Data.Automaton.Trans.Accum (runAccumS_)

tests = testGroup "Trans.Accum" [testCase "runAccumS_" $ runIdentity (embed (runAccumS_ (constM (add (Sum 1) >> look))) (replicate 5 ())) @?= (\n -> (n, n)) <$> [1, 2, 3, 4, 5]]
