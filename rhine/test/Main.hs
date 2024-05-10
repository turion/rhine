module Main where

-- tasty
import Test.Tasty

-- rhine
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
      ]
