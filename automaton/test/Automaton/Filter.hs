module Automaton.Filter where

-- base
import Control.Arrow
import Data.Functor.Identity (runIdentity)

-- tasty
import Test.Tasty

-- tasty-hunit
import Test.Tasty.HUnit

-- rhine

import Data.Automaton (embed)
import Data.Automaton.Filter

tests :: TestTree
tests =
  testGroup
    "Filter"
    [ testCase "Fizz Buzz" $ do
        let automaton3 = getFilterAutomaton $ filterS (\i -> i `mod` 3 == 0)
            automaton5 = getFilterAutomaton $ filterS (\i -> i `mod` 5 == 0)
            result = runIdentity $ embed (automaton3 &&& automaton5) [1 .. 10]
        result
          @?= [ (Nothing, Nothing)
              , (Nothing, Nothing)
              , (Just 3, Nothing)
              , (Nothing, Nothing)
              , (Nothing, Just 5)
              , (Just 6, Nothing)
              , (Nothing, Nothing)
              , (Nothing, Nothing)
              , (Just 9, Nothing)
              , (Nothing, Just 10)
              ]
    ]
