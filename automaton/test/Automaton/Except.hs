module Automaton.Except where

-- base
import Control.Monad.Identity (Identity (runIdentity))

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Data.Automaton (embed)
import Data.Automaton.Trans.Except (safe, safely, step)

tests = testGroup "Except" [testCase "step" $ runIdentity (embed (safely $ step (\a -> return (a, ())) >> safe 0) [1, 1, 1]) @?= [1, 0, 0]]
