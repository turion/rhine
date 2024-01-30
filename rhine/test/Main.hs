module Main where

-- tasty
import Test.Tasty

-- rhine
import Automaton
import Clock
import Schedule

main =
  defaultMain $
    testGroup
      "Main"
      [ Automaton.tests
      , Clock.tests
      , Schedule.tests
      ]
