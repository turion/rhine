module Main where

-- tasty
import Test.Tasty

-- rhine
import Automaton.MSF
import Clock
import Except
import Schedule

main =
  defaultMain $
    testGroup
      "Main"
      [ Clock.tests
      , Except.tests
      , Schedule.tests
      , Automaton.MSF.tests
      ]
