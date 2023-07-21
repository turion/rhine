module Main where

-- tasty
import Test.Tasty

-- rhine
import Clock
import Schedule

main =
  defaultMain $
    testGroup
      "Main"
      [ Clock.tests
      , Schedule.tests
      ]
