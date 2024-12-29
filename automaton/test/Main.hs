module Main where

-- tasty
import Test.Tasty

-- automaton
import Automaton
import Control.Monad.Trans.List
import Stream

main =
  defaultMain $
    testGroup
      "Main"
      [ Automaton.tests
      , Control.Monad.Trans.List.tests
      , Stream.tests
      ]
