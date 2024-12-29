module Test.Control.Monad.Trans.List where -- FIXME should I maybe move all test files in that hierarchy?

-- base

import Data.Automaton
import Data.Functor.Identity (Identity (runIdentity))

-- transformers
import Control.Monad.Trans.Writer.Strict (Writer)

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- transformers

-- automaton
import Control.Monad.Trans.List

tests =
  testGroup
    "Control.Monad.Trans.List"
    [ testGroup "Functor" $ _
    ]
