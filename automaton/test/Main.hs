module Main where

-- tasty
import Test.Tasty

-- automaton
import Automaton
import Stream

main =
  defaultMain $
    testGroup
      "Main"
      [ Automaton.tests
      , Stream.tests
      ]
