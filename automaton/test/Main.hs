module Main where

-- tasty
import Test.Tasty

-- automaton
import Automaton
import Test.Control.Monad.Trans.List
import Stream

main =
  defaultMain $
    testGroup
      "Main"
      [ Automaton.tests
      , Test.Control.Monad.Trans.List.tests
      , Stream.tests
      ]
